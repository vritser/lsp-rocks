import { ClientCapabilities, DidOpenTextDocumentNotification, DidOpenTextDocumentParams, DocumentSelector, RegistrationType, ServerCapabilities, TextDocumentItem, TextDocumentRegistrationOptions, DidChangeTextDocumentParams, DidChangeTextDocumentNotification } from "vscode-languageserver-protocol";
import { LanguageClient } from "../client";
import { ensure, RunnableDynamicFeature } from "./features";

export class DidOpenTextDocumentFeature extends RunnableDynamicFeature<TextDocumentItem, DidOpenTextDocumentParams, void, TextDocumentRegistrationOptions> {

  constructor(private client: LanguageClient) {
    super();
  }

  public fillClientCapabilities(capabilities: ClientCapabilities): void {
    ensure(ensure(capabilities, 'textDocument')!, 'synchronization')!.dynamicRegistration = true;
  }

  public initialize(capabilities: ServerCapabilities<any>, documentSelector: DocumentSelector | undefined): void {
    //
  }

  public createParams(req: TextDocumentItem): DidOpenTextDocumentParams {
    return { textDocument: req };
  }

  public runWith(params: DidOpenTextDocumentParams) {
    return this.client.sendNotification(this.registrationType.method, params);
  }

  public get registrationType(): RegistrationType<TextDocumentRegistrationOptions> {
		return DidOpenTextDocumentNotification.type;
	}

}

export class DidChangeTextDocumentFeature extends RunnableDynamicFeature<DidChangeTextDocumentParams, DidChangeTextDocumentParams, Promise<void>, TextDocumentRegistrationOptions> {

  private _fileVersions: Map<string, number>;

  constructor(private readonly client: LanguageClient) {
    super();
    this._fileVersions = new Map();
  }

  public fillClientCapabilities(capabilities: ClientCapabilities) {
    ensure(ensure(capabilities, 'textDocument')!, 'synchronization')!.dynamicRegistration = true;
  }

  public initialize(capabilities: ServerCapabilities<any>, documentSelector: DocumentSelector | undefined): void {
    //
  }

  protected createParams(params: DidChangeTextDocumentParams): DidChangeTextDocumentParams {
    return params;
  }

  private updateFileVersion(fileUri: string) {
    const version = this._fileVersions.get(fileUri) || 0;
    this._fileVersions.set(fileUri, version + 1);
    return version;
  }

  protected runWith(params: DidChangeTextDocumentParams) {
    return this.client.sendNotification(this.registrationType.method, params);
  }

  public get registrationType(): RegistrationType<TextDocumentRegistrationOptions> {
		return DidChangeTextDocumentNotification.type;
	}

}
