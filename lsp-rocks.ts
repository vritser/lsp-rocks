import { LanguageClient, TransportKind } from './client';
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

namespace Message {
  export function isResponse(msg: Message): msg is ResponseMessage {
    return emacsCommands.includes(msg.cmd as EmacsCommand);
  }
}

type RequestId = string;

interface Message {
  id: RequestId,
  cmd: ServerCommand | EmacsCommand,
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

  constructor(serverPort: number) {
    this._serverPort = serverPort;
    this._clients = new Map();
  }

  public start() {
    this._server = new WebSocketServer({ port: this._serverPort })
      .on('connection', async (ws: WebSocket) => {
        ws.id = randomUUID();

        ws.on('message', async (msg: string) => {
          const message = JSON.parse(msg) as Message;
          await this.messageHandler(ws, message);
        });

        ws.on('close', () => {
          console.log('a connection closed');
        });
      });
  }

  public async messageHandler(socket: WebSocket, msg: Message) {
    const { id, cmd } = msg;
    console.log(`message =========> id: ${msg.id}, cmd: ${msg.cmd}, params: ${JSON.stringify((msg as any).params)}`);
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

      data = await client.on(req.cmd, req.params);
      if (data != null) {
        socket.send(mkres(id, cmd, data));
      }
    }

  }

  private async ensureClient(clientId: string, params?: { language: string, project: string, command: string }): Promise<LanguageClient> {
    let client = this._clients.get(clientId);

    if (client === undefined) {
      if (params != undefined) {
        client = new LanguageClient(params.language, params.project, {}, {
          command: params.command,
          transport: TransportKind.stdio,
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