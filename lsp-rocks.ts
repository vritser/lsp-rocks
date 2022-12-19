import { LanguageClient } from './client';
import { Server, WebSocket, WebSocketServer } from 'ws';
import { randomUUID } from 'crypto';

/**
 * All supports request commands
 */
enum ServerCommand {
  Init = 'init',
}

enum EmacsCommand {
  GetVar = 'get-var',
  CallFunc = 'call-func',
}

const emacsCommands = Object.values(EmacsCommand);

interface InitParams {
  language: string;
  project: string;
  command: string;
  args: string[];
  clientInfo: { name: string, version: string };
}

namespace Message {
  export function isResponse(msg: Message): msg is ResponseMessage {
    return emacsCommands.includes(msg.cmd as EmacsCommand);
  }
}

type RequestId = string;

interface Message {
  id: RequestId,
  cmd: string | ServerCommand | EmacsCommand,
}

interface RequestMessage extends Message {
  lang: string;
  project: string;
  params: any;
}

interface ResponseMessage extends Message {
  data: any;
}


function mkres(id: string | number, cmd: string, data: string[]) {
  return JSON.stringify({ id, cmd, data });
}

export class LspRocks {
  private _server: Server;

  private _serverPort: number;

  private _emacsVars: Map<string, any>;

  readonly _clients: Map<string, LanguageClient>;

  readonly _recentRequests: Map<string, any>;

  constructor(serverPort: number) {
    this._serverPort = serverPort;
    this._clients = new Map();
    this._recentRequests = new Map();
  }

  public start() {
    this._server = new WebSocketServer({ port: this._serverPort })
      .on('connection', async (ws: WebSocket) => {
        ws.id = randomUUID();

        ws.on('message', async (msg: string) => {
          const message = JSON.parse(msg) as Message;
          this._recentRequests.set(message.cmd, message.id);
          await this.messageHandler(ws, message);
        });

        ws.on('close', () => {
          console.log('a connection closed');
        });
      });
  }

  public async messageHandler(socket: WebSocket, msg: Message) {
    const { id, cmd } = msg;
    console.log(`receive message => id: ${msg.id}, cmd: ${msg.cmd}, params: ${JSON.stringify((msg as any).params)}`);
    const logLabel = `${id}:${cmd}`;
    console.time(logLabel)
    if (Message.isResponse(msg)) {
      // TODO
    } else {
      const req = msg as RequestMessage;
      let data: any = null;
      // eslint-disable-next-line @typescript-eslint/no-non-null-assertion
      const client = await this.ensureClient(socket.id!, req.params);

      if (req.cmd == ServerCommand.Init) {
        return;
      }

      if (this._recentRequests.get(req.cmd) != req.id && req.cmd != 'textDocument/didChange') {
        return;
      }
      data = await client.on(req.cmd, req.params);
      if (this._recentRequests.get(req.cmd) != req.id) {
        return;
      }
      console.timeLog(logLabel)
      if (data != null) {
        socket.send(mkres(id, cmd, data));
      }
    }

  }

  private async ensureClient(clientId: string, params?: InitParams): Promise<LanguageClient> {
    let client = this._clients.get(clientId);
    if (client === undefined) {
      if (params != undefined) {
        client = new LanguageClient(params.language, params.project, params.clientInfo, {
          command: params.command,
          args: params.args,
          options: { cwd: params.project },
        });
        this._clients.set(clientId, client);
        await client.start();
      } else {
        throw new Error('Can not create LanguageClient, because language and project is undefined');
      }
    }

    return client;
  }

}