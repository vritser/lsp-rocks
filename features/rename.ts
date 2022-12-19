import { ClientCapabilities, PrepareRenameParams, PrepareRenameRequest, PrepareRenameResult, PrepareSupportDefaultBehavior, RegistrationType, RenameParams, RenameRegistrationOptions, RenameRequest, TextDocumentEdit, WorkspaceEdit } from "vscode-languageserver-protocol";
import { LanguageClient } from "../client";
import { RunnableDynamicFeature, ensure } from "./features";

export class RenameFeature extends RunnableDynamicFeature<RenameParams, RenameParams, Promise<WorkspaceEdit | null>, RenameRegistrationOptions> {

  constructor(private client: LanguageClient) {
    super();
  }

  public fillClientCapabilities(capabilities: ClientCapabilities): void {
    const rename = ensure(ensure(capabilities, 'textDocument')!, 'rename')!;
    rename.dynamicRegistration = true;
    rename.prepareSupport = true;
    rename.prepareSupportDefaultBehavior = PrepareSupportDefaultBehavior.Identifier;
    rename.honorsChangeAnnotations = true;
  }

  public async runWith(params: RenameParams): Promise<WorkspaceEdit | null> {
    const resp = await this.client.sendRequest(RenameRequest.type, params);
    if (resp == null) return null;

    console.log('rename resp ==> ', JSON.stringify(resp));

    if (resp.changes != undefined) {
      if (resp.documentChanges == undefined) {
        resp.documentChanges = [];
      }
      resp.documentChanges = resp.documentChanges.concat(Object.keys(resp.changes)
        .map(uri => {
          return {
            textDocument: { uri, },
            edits: resp.changes?.[uri],
          } as TextDocumentEdit;
        }));
    }

    return resp;
  }

  public get registrationType(): RegistrationType<RenameRegistrationOptions> {
    return RenameRequest.type;
  }

}

export class PrepareRenameFeature extends RunnableDynamicFeature<PrepareRenameParams, PrepareRenameParams, Promise<PrepareRenameResult | null>, void> {

  constructor(private client: LanguageClient) {
    super();
  }

  public fillClientCapabilities(capabilities: ClientCapabilities): void {
    const rename = ensure(ensure(capabilities, 'textDocument')!, 'rename')!;
    rename.prepareSupport = true;
    rename.prepareSupportDefaultBehavior = PrepareSupportDefaultBehavior.Identifier;
  }

  public async runWith(params: PrepareRenameParams): Promise<PrepareRenameResult | null> {
    const resp = await this.client.sendRequest(PrepareRenameRequest.type, params);
    if (resp == null) return null;

    console.log('prepare resp ==> ', resp);

    if ('defaultBehavior' in resp) {
      return null;
    }

    if ('start' in resp) {
      return {
        range: { start: resp.start, end: resp.end },
        placeholder: '',
      };
    }

    return resp;
  }

  public get registrationType(): RegistrationType<void> {
    return PrepareRenameRequest.type;
  }

}