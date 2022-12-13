import { ClientCapabilities, DidOpenTextDocumentNotification, DidOpenTextDocumentParams, RegistrationType, TextDocumentRegistrationOptions, DidChangeTextDocumentParams, DidChangeTextDocumentNotification, WillSaveTextDocumentParams, WillSaveTextDocumentNotification, DidSaveTextDocumentParams, DidSaveTextDocumentNotification, DidCloseTextDocumentNotification, DidCloseTextDocumentParams } from "vscode-languageserver-protocol";
import { LanguageClient } from "../client";
import { ensure, RunnableDynamicFeature } from "./features";

export class DidOpenTextDocumentFeature extends RunnableDynamicFeature<DidOpenTextDocumentParams, DidOpenTextDocumentParams, void, TextDocumentRegistrationOptions> {

  constructor(private client: LanguageClient) {
    super();
  }

  public fillClientCapabilities(capabilities: ClientCapabilities): void {
    ensure(ensure(capabilities, 'textDocument')!, 'synchronization')!.dynamicRegistration = true;
  }

  public runWith(params: DidOpenTextDocumentParams) {
    return this.client.sendNotification(this.registrationType.method, params);
  }

  public get registrationType(): RegistrationType<TextDocumentRegistrationOptions> {
    return DidOpenTextDocumentNotification.type;
  }

}

export class DidCloseTextDocumentFeature extends RunnableDynamicFeature<DidCloseTextDocumentParams, DidOpenTextDocumentParams, void, TextDocumentRegistrationOptions> {

  constructor(private client: LanguageClient) {
    super();
  }

  public fillClientCapabilities(capabilities: ClientCapabilities): void {
    ensure(ensure(capabilities, 'textDocument')!, 'synchronization')!.dynamicRegistration = true;
  }

  public runWith(params: DidCloseTextDocumentParams) {
    return this.client.sendNotification(this.registrationType.method, params);
  }

  public get registrationType(): RegistrationType<TextDocumentRegistrationOptions> {
    return DidCloseTextDocumentNotification.type;
  }

}

export class DidChangeTextDocumentFeature extends RunnableDynamicFeature<DidChangeTextDocumentParams, DidChangeTextDocumentParams, Promise<void>, TextDocumentRegistrationOptions> {

  constructor(private readonly client: LanguageClient) {
    super();
  }

  public fillClientCapabilities(capabilities: ClientCapabilities) {
    ensure(ensure(capabilities, 'textDocument')!, 'synchronization')!.dynamicRegistration = true;
  }

  protected runWith(params: DidChangeTextDocumentParams) {
    return this.client.sendNotification(this.registrationType.method, params);
  }

  public get registrationType(): RegistrationType<TextDocumentRegistrationOptions> {
    return DidChangeTextDocumentNotification.type;
  }

}

export class WillSaveTextDocumentFeature extends RunnableDynamicFeature<WillSaveTextDocumentParams, WillSaveTextDocumentParams, Promise<void>, TextDocumentRegistrationOptions> {

  constructor(private readonly client: LanguageClient) {
    super();
  }

  public fillClientCapabilities(capabilities: ClientCapabilities) {
    ensure(ensure(capabilities, 'textDocument')!, 'synchronization')!.willSave = true;
  }

  protected createParams(params: WillSaveTextDocumentParams): WillSaveTextDocumentParams {
    return params;
  }

  protected runWith(params: WillSaveTextDocumentParams) {
    return this.client.sendNotification(this.registrationType.method, params);
  }

  public get registrationType(): RegistrationType<TextDocumentRegistrationOptions> {
    return WillSaveTextDocumentNotification.type;
  }

}

export class DidSaveTextDocumentFeature extends RunnableDynamicFeature<DidSaveTextDocumentParams, DidSaveTextDocumentParams, Promise<void>, TextDocumentRegistrationOptions> {

  constructor(private readonly client: LanguageClient) {
    super();
  }

  public fillClientCapabilities(capabilities: ClientCapabilities) {
    ensure(ensure(capabilities, 'textDocument')!, 'synchronization')!.didSave = true;
  }

  protected runWith(params: DidSaveTextDocumentParams) {
    return this.client.sendNotification(this.registrationType.method, params);
  }

  public get registrationType(): RegistrationType<TextDocumentRegistrationOptions> {
    return DidSaveTextDocumentNotification.type;
  }

}
