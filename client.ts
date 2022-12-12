
import * as Is from './util';
import * as fs from 'fs';
import { ChildProcess, spawn } from 'child_process';

import {
  CancellationToken, ClientCapabilities, DiagnosticTag, DidChangeTextDocumentNotification,
  ErrorCodes, ExitNotification, FailureHandlingKind, InitializeParams,
  InitializeRequest, InitializeResult, InitializedNotification, LogMessageNotification,
  MessageType, NotificationType, NotificationType0, PositionEncodingKind,
  ProtocolConnection, ProtocolNotificationType, ProtocolNotificationType0, ProtocolRequestType,
  ProtocolRequestType0, RequestType, RequestType0, ResourceOperationKind, SemanticTokensDeltaRequest,
  SemanticTokensRangeRequest, SemanticTokensRequest, ServerCapabilities, ShutdownRequest, TextDocumentSyncKind,
  TextDocumentSyncOptions, createProtocolConnection,
} from 'vscode-languageserver-protocol';

import { CancellationStrategy, ConnectionOptions, Message, MessageReader, MessageSignature, MessageWriter, RAL, ResponseError, StreamMessageReader, StreamMessageWriter, generateRandomPipeName } from 'vscode-jsonrpc/node';
import { DynamicFeature, ensure, RunnableDynamicFeature } from './features/features';
import { DidChangeTextDocumentFeature, DidOpenTextDocumentFeature } from './features/textSynchronization';
import { CompletionFeature, CompletionItemResolveFeature } from './features/completion';
import { DefinitionFeature } from './features/definition';
import { DeclarationFeature } from './features/declaration';


enum ClientState {
  Initial = 'initial',
  Starting = 'starting',
  StartFailed = 'startFailed',
  Running = 'running',
  Stopping = 'stopping',
  Stopped = 'stopped',
}

/**
   * Signals in which state the language client is in.
   */
export enum State {
  /**
     * The client is stopped or got never started.
     */
  Stopped = 1,
  /**
     * The client is starting but not ready yet.
     */
  Starting = 3,
  /**
     * The client is running and ready.
     */
  Running = 2,
}

export enum TransportKind {
  stdio,
  ipc,
  pipe,
  socket,
}

export interface SocketTransport {
  kind: TransportKind.socket;
  port: number;
}

/**
 * To avoid any timing, pipe name or port number issues the pipe (TransportKind.pipe)
 * and the sockets (TransportKind.socket and SocketTransport) are owned by the
 * VS Code processes. The server process simply connects to the pipe / socket.
 * In node term the VS Code process calls `createServer`, then starts the server
 * process, waits until the server process has connected to the pipe / socket
 * and then signals that the connection has been established and messages can
 * be send back and forth. If the language server is implemented in a different
 * program language the server simply needs to create a connection to the
 * passed pipe name or port number.
 */
export type Transport = TransportKind | SocketTransport;

namespace Transport {
  export function isSocket(value: Transport | undefined): value is SocketTransport {
    const candidate = value as SocketTransport;
    return candidate && candidate.kind === TransportKind.socket && Is.number(candidate.port);
  }
}

export type ServerOptions = Executable;

export interface ExecutableOptions {
  cwd?: string;
  env?: any;
  detached?: boolean;
  shell?: boolean;
}

export interface Executable {
  command: string;
  transport?: Transport;
  args?: string[];
  options?: ExecutableOptions;
}

namespace Executable {
  export function is(value: any): value is Executable {
    return Is.string(value.command);
  }
}

export interface MessageTransports {
  reader: MessageReader;
  writer: MessageWriter;
  detached?: boolean;
}

export namespace MessageTransports {
  export function is(value: any): value is MessageTransports {
    const candidate: MessageTransports = value;
    return candidate && MessageReader.is(value.reader) && MessageWriter.is(value.writer);
  }
}

type ResolvedClientOptions = {
  stdioEncoding: string;
  initializationOptions?: any | (() => any);
  progressOnInitialization: boolean;
  connectionOptions?: {
    cancellationStrategy?: CancellationStrategy;
    maxRestartCount?: number;
  };
  markdown: {
    isTrusted: boolean;
    supportHtml: boolean;
  };
};

interface ConnectionErrorHandler {
  (error: Error, message: Message | undefined, count: number | undefined): void;
}

interface ConnectionCloseHandler {
  (): void;
}

function createConnection(
  input: MessageReader,
  output: MessageWriter,
  errorHandler: ConnectionErrorHandler,
  closeHandler: ConnectionCloseHandler,
  options?: ConnectionOptions,
): ProtocolConnection {
  const connection = createProtocolConnection(input, output, console, options);
  connection.onError((data) => { errorHandler(data[0], data[1], data[2]); });
  connection.onClose(closeHandler);
  return connection;
}

function handleChildProcessStartError(process: ChildProcess, message: string) {
  if (process === null) {
    return Promise.reject<MessageTransports>(message);
  }

  return new Promise<MessageTransports>((_, reject) => {
    process.on('error', (err) => {
      reject(`${message} ${err}`);
    });
    // the error event should always be run immediately,
    // but race on it just in case
    setImmediate(() => reject(message));
  });
}


export class LanguageClient {

  readonly _project: string;

  readonly _language: string;

  readonly _name: string;

  private readonly _serverOptions: ServerOptions;

  private _serverProcess: ChildProcess | undefined;

  private _state: ClientState;

  private _onStart: Promise<void> | undefined;

  private _onStop: Promise<void> | undefined;

  private _connection: ProtocolConnection | undefined;

  private _clientInfo: any;

  private _initializeResult: InitializeResult | undefined;

  private _capabilities!: ServerCapabilities;

  private _clientOptions: ResolvedClientOptions;

  private _fileVersions: Map<string, number>;

  private _features: DynamicFeature<any>[];

  private _dynamicFeatures: Map<string, DynamicFeature<any>>;

  constructor(language: string, project: string, clientInfo: any, serverOptions: ServerOptions) {
    this._name = `${project}:${language}`;
    this._project = project;
    this._language = language;
    this._clientInfo = clientInfo;
    this._serverOptions = serverOptions;
    this._fileVersions = new Map();
    this._features = [];
    this._dynamicFeatures = new Map();
    this._clientOptions = {
      stdioEncoding: 'utf-8',
      progressOnInitialization: false,
      markdown: {
        isTrusted: true,
        supportHtml: true,
      },
    };

    this.registerBuiltinFeatures();
  }

  private get $state(): ClientState {
    return this._state;
  }

  private set $state(value: ClientState) {
    this._state = value;
  }

  public sendRequest<R, PR, E, RO>(type: ProtocolRequestType0<R, PR, E, RO>, token?: CancellationToken): Promise<R>;
  public sendRequest<P, R, PR, E, RO>(type: ProtocolRequestType<P, R, PR, E, RO>, params: P, token?: CancellationToken): Promise<R>;
  public sendRequest<R, E>(type: RequestType0<R, E>, token?: CancellationToken): Promise<R>;
  public sendRequest<P, R, E>(type: RequestType<P, R, E>, params: P, token?: CancellationToken): Promise<R>;
  public sendRequest<R>(method: string, token?: CancellationToken): Promise<R>;
  public sendRequest<R>(method: string, param: any, token?: CancellationToken): Promise<R>;
  public async sendRequest<R>(type: string | MessageSignature, ...params: any[]): Promise<R> {
    if (this.$state === ClientState.StartFailed || this.$state === ClientState.Stopping || this.$state === ClientState.Stopped) {
      return Promise.reject(new ResponseError(ErrorCodes.ConnectionInactive, 'Client is not running'));
    }
    try {
      // Ensure we have a connection before we force the document sync.
      const connection = await this.$start();
      // await this.forceDocumentSync();
      return await connection.sendRequest<R>(type, ...params);
    } catch (error) {
      this.error(`Sending request ${Is.string(type) ? type : type.method} failed.`, error);
      throw error;
    }
  }


  public sendNotification<RO>(type: ProtocolNotificationType0<RO>): Promise<void>;
  public sendNotification<P, RO>(type: ProtocolNotificationType<P, RO>, params?: P): Promise<void>;
  public sendNotification(type: NotificationType0): Promise<void>;
  public sendNotification<P>(type: NotificationType<P>, params?: P): Promise<void>;
  public sendNotification(method: string): Promise<void>;
  public sendNotification(method: string, params: any): Promise<void>;
  public async sendNotification<P>(type: string | MessageSignature, params?: P): Promise<void> {
    if (this.$state === ClientState.StartFailed || this.$state === ClientState.Stopping || this.$state === ClientState.Stopped) {
      return Promise.reject(new ResponseError(ErrorCodes.ConnectionInactive, 'Client is not running'));
    }
    try {
      // Ensure we have a connection before we force the document sync.
      const connection = await this.$start();
      return await connection.sendNotification(type, params);
    } catch (error) {
      this.error(`Sending notification ${Is.string(type) ? type : type.method} failed.`, error);
      throw error;
    }
  }

  public async restart(): Promise<void> {
    await this.stop();
    await this.start();
  }


  public async start(): Promise<void> {
    if (this.$state === ClientState.Stopping) {
      throw new Error('Client is currently stopping. Can only restart a full stopped client');
    }
    // We are already running or are in the process of getting up
    // to speed.
    if (this._onStart !== undefined) {
      return this._onStart;
    }
    const [promise, resolve, reject] = this.createOnStartPromise();
    this._onStart = promise;

    this.$state = ClientState.Starting;
    try {
      const connection = await this.createConnection();
      connection.onNotification(LogMessageNotification.type, (message) => {
        switch (message.type) {
          case MessageType.Error:
            this.error(message.message);
            break;
          case MessageType.Warning:
            this.warn(message.message);
            break;
          case MessageType.Info:
            this.info(message.message);
            break;
          default:
            console.log(message.message);
        }
      });
      connection.listen();
      await this.initialize(connection, this._clientInfo);
      resolve();
    } catch (error) {
      this.$state = ClientState.StartFailed;
      this.error(`${this._name} client: couldn't create connection to server.`, error);
      reject(error);
    }
    return this._onStart;
  }

  private async initialize(connection: ProtocolConnection, clientInfo: any): Promise<InitializeResult> {
    // May language server need some initialization options.
    const langserverConfig = `./langserver/${this._language}`;
    const initializationOptions = fs.existsSync(langserverConfig) ? require(langserverConfig) : {};
    const initParams: InitializeParams = {
      processId: null,
      clientInfo,
      locale: 'en',
      rootPath: this._project,
      rootUri: `file://${this._project}`,
      capabilities: this.computeClientCapabilities(),
      initializationOptions,
      workspaceFolders: [{
        uri: `file://${this._project}`,
        name: this._project.slice(this._project.lastIndexOf('/')),
      }],
    };
    this.fillInitializeParams(initParams);
    return this.doInitialize(connection, initParams);
  }

  public registerFeature(feature: DynamicFeature<any>): void {
		this._features.push(feature);
		if (DynamicFeature.is(feature)) {
			const registrationType = feature.registrationType;
			this._dynamicFeatures.set(registrationType.method, feature);
		}
	}

  protected registerBuiltinFeatures() {
		this.registerFeature(new DidOpenTextDocumentFeature(this));
		this.registerFeature(new DidChangeTextDocumentFeature(this));
    this.registerFeature(new CompletionFeature(this));
    this.registerFeature(new CompletionItemResolveFeature(this));
    this.registerFeature(new DefinitionFeature(this));
    this.registerFeature(new DeclarationFeature(this));
  }

  protected fillInitializeParams(params: InitializeParams): void {
		for (const feature of this._features) {
			if (Is.func(feature.fillInitializeParams)) {
				feature.fillInitializeParams(params);
			}
		}
	}

  private computeClientCapabilities(): ClientCapabilities {
    const result: ClientCapabilities = {};
    ensure(result, 'workspace')!.applyEdit = true;

    const workspaceEdit = ensure(ensure(result, 'workspace')!, 'workspaceEdit')!;
    workspaceEdit.documentChanges = true;
    workspaceEdit.resourceOperations = [ResourceOperationKind.Create, ResourceOperationKind.Rename, ResourceOperationKind.Delete];
    workspaceEdit.failureHandling = FailureHandlingKind.TextOnlyTransactional;
    workspaceEdit.normalizesLineEndings = true;
    workspaceEdit.changeAnnotationSupport = {
      groupsOnLabel: true,
    };

    const diagnostics = ensure(ensure(result, 'textDocument')!, 'publishDiagnostics')!;
    diagnostics.relatedInformation = true;
    diagnostics.versionSupport = false;
    diagnostics.tagSupport = { valueSet: [DiagnosticTag.Unnecessary, DiagnosticTag.Deprecated] };
    diagnostics.codeDescriptionSupport = true;
    diagnostics.dataSupport = true;

    const windowCapabilities = ensure(result, 'window')!;
    const showMessage = ensure(windowCapabilities, 'showMessage')!;
    showMessage.messageActionItem = { additionalPropertiesSupport: true };
    const showDocument = ensure(windowCapabilities, 'showDocument')!;
    showDocument.support = true;

    const generalCapabilities = ensure(result, 'general')!;
    generalCapabilities.staleRequestSupport = {
      cancel: true,
      retryOnContentModified: Array.from(LanguageClient.RequestsToCancelOnContentModified),
    };
    generalCapabilities.regularExpressions = { engine: 'ECMAScript', version: 'ES2020' };
    generalCapabilities.markdown = {
      parser: 'marked',
      version: '1.1.0',
    };
    generalCapabilities.positionEncodings = ['utf-16'];

    // if (this._clientOptions.markdown.supportHtml) {
    // eslint-disable-next-line max-len
    // 	generalCapabilities.markdown.allowedTags = ['ul', 'li', 'p', 'code', 'blockquote', 'ol', 'h1', 'h2', 'h3', 'h4', 'h5', 'h6', 'hr', 'em', 'pre', 'table', 'thead', 'tbody', 'tr', 'th', 'td', 'div', 'del', 'a', 'strong', 'br', 'img', 'span'];
    // }

    for (const feature of this._features) {
      feature.fillClientCapabilities(result);
    }

    return result;
  }

  private async doInitialize(connection: ProtocolConnection, initParams: InitializeParams): Promise<InitializeResult> {
    try {
      const result = await connection.sendRequest(InitializeRequest.type, initParams);
      if (result.capabilities.positionEncoding !== undefined && result.capabilities.positionEncoding !== PositionEncodingKind.UTF16) {
        throw new Error(`Unsupported position encoding (${result.capabilities.positionEncoding}) received from server ${this._name}`);
      }

      console.log('init resp ===>', result);
      this._initializeResult = result;
      this.$state = ClientState.Running;

      let textDocumentSyncOptions: TextDocumentSyncOptions | undefined = undefined;
      if (Is.number(result.capabilities.textDocumentSync)) {
        if (result.capabilities.textDocumentSync === TextDocumentSyncKind.None) {
          textDocumentSyncOptions = {
            openClose: false,
            change: TextDocumentSyncKind.None,
            save: undefined,
          };
        } else {
          textDocumentSyncOptions = {
            openClose: true,
            change: result.capabilities.textDocumentSync,
            save: {
              includeText: false,
            },
          };
        }
      } else if (result.capabilities.textDocumentSync !== undefined && result.capabilities.textDocumentSync !== null) {
        textDocumentSyncOptions = result.capabilities.textDocumentSync as TextDocumentSyncOptions;
      }
      this._capabilities = Object.assign({}, result.capabilities, { resolvedTextDocumentSync: textDocumentSyncOptions });

      await connection.sendNotification(InitializedNotification.type, {});

      return result;
    } catch (error) {
      this.error('Server initialization failed.', error);
      void this.stop();
      throw error;
    }
  }

  private async $start(): Promise<ProtocolConnection> {
    if (this.$state === ClientState.StartFailed) {
      throw new Error('Previous start failed. Can\'t restart server.');
    }
    await this.start();

    const connection = this.activeConnection();
    if (connection === undefined) {
      throw new Error('Starting server failed');
    }
    return connection;
  }

  private activeConnection(): ProtocolConnection | undefined {
    return this.$state === ClientState.Running && this._connection !== undefined ? this._connection : undefined;
  }

  public stop(timeout = 2000): Promise<void> {
    return this.shutdown('stop', timeout).finally(() => {
      if (this._serverProcess) {
        this._serverProcess = undefined;
      }
    });
  }

  private async shutdown(mode: 'suspend' | 'stop', timeout: number): Promise<void> {
    // If the client is stopped or in its initial state return.
    if (this.$state === ClientState.Stopped || this.$state === ClientState.Initial) {
      return;
    }

    // If we are stopping the client and have a stop promise return it.
    if (this.$state === ClientState.Stopping) {
      if (this._onStop !== undefined) {
        return this._onStop;
      } else {
        throw new Error('Client is stopping but no stop promise available.');
      }
    }

    const connection = this.activeConnection();

    // We can't stop a client that is not running (e.g. has no connection). Especially not
    // on that us starting since it can't be correctly synchronized.
    if (connection === undefined || this.$state !== ClientState.Running) {
      throw new Error(`Client is not running and can't be stopped. It's current state is: ${this.$state}`);
    }

    this._initializeResult = undefined;
    this.$state = ClientState.Stopping;

    const tp = new Promise<undefined>(c => { RAL().timer.setTimeout(c, timeout); });
    // eslint-disable-next-line @typescript-eslint/no-shadow
    const shutdown = (async (connection) => {
      await connection.sendRequest(ShutdownRequest.type, undefined);
      await connection.sendNotification(ExitNotification.type);
      return connection;
    })(connection);

    // eslint-disable-next-line @typescript-eslint/no-shadow
    return this._onStop = Promise.race([tp, shutdown]).then((connection) => {
      // The connection won the race with the timeout.
      if (connection !== undefined) {
        connection.end();
        connection.dispose();
      } else {
        this.error('Stopping server timed out');
        throw new Error('Stopping the server timed out');
      }
    }, (error) => {
      this.error('Stopping server failed', error);
      throw error;
    }).finally(() => {
      this.$state = ClientState.Stopped;
      this._onStart = undefined;
      this._onStop = undefined;
      this._connection = undefined;
    });
  }

  private createOnStartPromise(): [Promise<void>, () => void, (error: any) => void] {
    let resolve!: () => void;
    let reject!: (error: any) => void;
    const promise: Promise<void> = new Promise((_resolve, _reject) => {
      resolve = _resolve;
      reject = _reject;
    });
    return [promise, resolve, reject];
  }

  // TODO
  private async createConnection(): Promise<ProtocolConnection> {
    const errorHandler = () => {
      //
    };

    const closeHandler = () => {
      //
    };

    const transports = await this.createMessageTransports('utf8');
    this._connection = createConnection(transports.reader, transports.writer, errorHandler, closeHandler, this._clientOptions.connectionOptions);
    return this._connection;
  }

  public async on(method: string, params: any) {
    return (this._dynamicFeatures.get(method) as RunnableDynamicFeature<any, any, any, any>).run(params)
  }

  public async didChange(params: any) {
    this.sendNotification(DidChangeTextDocumentNotification.type, {
      textDocument: {
        uri: params.uri,
        version: this.updateFileVersion(params.uri),
      },
      contentChanges: [params.contentChange],
    });
  }

  private updateFileVersion(fileUri: string) {
    const version = this._fileVersions.get(fileUri) || 0;
    this._fileVersions.set(fileUri, version + 1);
    return version;
  }

  protected createMessageTransports(encoding: string): Promise<MessageTransports> {

    const server = this._serverOptions;
    return this._getServerWorkingDir(server.options).then(serverWorkingDir => {
      if (Executable.is(server) && server.command) {
        const args: string[] = server.args !== undefined ? server.args.slice(0) : [];
        let pipeName: string | undefined = undefined;
        const transport = server.transport;
        if (transport === TransportKind.stdio) {
          args.push('--stdio');
        } else if (transport === TransportKind.pipe) {
          pipeName = generateRandomPipeName();
          args.push(`--pipe=${pipeName}`);
        } else if (Transport.isSocket(transport)) {
          args.push(`--socket=${transport.port}`);
        } else if (transport === TransportKind.ipc) {
          throw new Error('Transport kind ipc is not support for command executable');
        }
        const options = Object.assign({}, server.options);
        options.cwd = options.cwd || serverWorkingDir;
        if (transport === undefined || transport === TransportKind.stdio) {
          const serverProcess = spawn(server.command, args, options);
          if (!serverProcess || !serverProcess.pid) {
            return handleChildProcessStartError(serverProcess, `Launching server using command ${server.command} failed.`);
          }
          serverProcess.stderr.on('data', data => console.error('server error: ', Is.string(data) ? data : data.toString(encoding)));
          this._serverProcess = serverProcess;
          return Promise.resolve({ reader: new StreamMessageReader(serverProcess.stdout), writer: new StreamMessageWriter(serverProcess.stdin) });
        }
      }
      return Promise.reject<MessageTransports>(new Error('Unsupported server configuration ' + JSON.stringify(server, null, 4)));
    });
  }

  // TODO
  private _getServerWorkingDir(options?: { cwd?: string }): Promise<string | undefined> {
    let cwd = options && options.cwd;
    if (!cwd) {
      cwd = undefined;
    }
    if (cwd) {
      // make sure the folder exists otherwise creating the process will fail
      return new Promise(s => {
        fs.lstat(cwd!, (err, stats) => {
          s(!err && stats.isDirectory() ? cwd : undefined);
        });
      });
    }
    return Promise.resolve(undefined);
  }

  private data2String(data: object): string {
    if (data instanceof ResponseError) {
      const responseError = data as ResponseError<any>;
      return `  Message: ${responseError.message}\n  Code: ${responseError.code} ${responseError.data ? '\n' + responseError.data.toString() : ''}`;
    }
    if (data instanceof Error) {
      if (Is.string(data.stack)) {
        return data.stack;
      }
      return (data as Error).message;
    }
    if (Is.string(data)) {
      return data;
    }
    return data.toString();
  }

  public info(message: string, data?: any): void {
    console.info(`[Info  - ${(new Date().toLocaleTimeString())}] ${message || this.data2String(data)}`);
  }

  public warn(message: string, data?: any): void {
    console.warn(`[Warn  - ${(new Date().toLocaleTimeString())}] ${message || this.data2String(data)}`);
  }

  public error(message: string, data?: any): void {
    console.error(`[Error  - ${(new Date().toLocaleTimeString())}] ${message || this.data2String(data)}`);
  }

  private static RequestsToCancelOnContentModified: Set<string> = new Set([
    SemanticTokensRequest.method,
    SemanticTokensRangeRequest.method,
    SemanticTokensDeltaRequest.method,
  ]);

}