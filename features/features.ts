import { InitializeParams, ClientCapabilities, ServerCapabilities, DocumentSelector, RegistrationType, TextDocumentRegistrationOptions, ProtocolNotificationType } from "vscode-languageserver-protocol";
import { LanguageClient } from "../client";
import * as Is from '../util';


export function ensure<T, K extends keyof T>(target: T, key: K): T[K] {
  if (target[key] === undefined) {
    target[key] = {} as any;
  }
  return target[key];
}

/**
 * A dynamic feature can be activated via the server.
 */
export interface DynamicFeature<RO> {

  /**
   * Called to fill the initialize params.
   *
   * @params the initialize params.
   */
  fillInitializeParams?: (params: InitializeParams) => void;

  /**
   * Called to fill in the client capabilities this feature implements.
   *
   * @param capabilities The client capabilities to fill.
   */
  fillClientCapabilities(capabilities: ClientCapabilities): void;

  /**
   * A preflight where the server capabilities are shown to all features
   * before a feature is actually initialized. This allows feature to
   * capture some state if they are a pre-requisite for other features.
   *
   * @param capabilities the server capabilities
   * @param documentSelector the document selector pass to the client's constructor.
   *  May be `undefined` if the client was created without a selector.
   */
  preInitialize?: (capabilities: ServerCapabilities, documentSelector: DocumentSelector | undefined) => void;

  /**
   * Initialize the feature. This method is called on a feature instance
   * when the client has successfully received the initialize request from
   * the server and before the client sends the initialized notification
   * to the server.
   *
   * @param capabilities the server capabilities.
   * @param documentSelector the document selector pass to the client's constructor.
   *  May be `undefined` if the client was created without a selector.
   */
  initialize(capabilities: ServerCapabilities, documentSelector: DocumentSelector | undefined): void;

  /**
   * The signature (e.g. method) for which this features support dynamic activation / registration.
   */
  registrationType: RegistrationType<RO>;
}

export namespace DynamicFeature {
	export function is<T>(value: any): value is DynamicFeature<T> {
		const candidate: DynamicFeature<T> = value;
		return candidate !== undefined && candidate !== null &&
			Is.func(candidate.fillClientCapabilities) && Is.func(candidate.initialize) &&
			(candidate.fillInitializeParams === undefined || Is.func(candidate.fillInitializeParams)) &&
			candidate.registrationType !== undefined;
	}
}

export abstract class RunnableDynamicFeature<R, P, A, RO> implements DynamicFeature<RO> {

  protected createParams(params: R): P {
    return params as unknown as P;
  }

  protected abstract runWith(params: P): A;

  public run(params: R): A {
    return this.runWith(this.createParams(params));
  }

  // Repeat from interface.
  public abstract fillClientCapabilities(capabilities: ClientCapabilities): void;
  public initialize(capabilities: ServerCapabilities, documentSelector: DocumentSelector | undefined): void {
    // default impl
  }
  public abstract registrationType: RegistrationType<RO>;
}

/**
 * An abstract dynamic feature implementation that operates on documents (e.g. text
 * documents or notebooks).
 */
export abstract class DynamicDocumentFeature<RO> implements DynamicFeature<RO> {

  protected readonly _client: LanguageClient;

  constructor(client: LanguageClient) {
    this._client = client;
  }

  // Repeat from interface.
  public abstract fillClientCapabilities(capabilities: ClientCapabilities): void;
  public abstract initialize(capabilities: ServerCapabilities, documentSelector: DocumentSelector | undefined): void;
  public abstract registrationType: RegistrationType<RO>;
}

export abstract class TextDocumentEventFeature<P, A, R> extends DynamicDocumentFeature<TextDocumentRegistrationOptions> {
  protected readonly _type: ProtocolNotificationType<P, TextDocumentRegistrationOptions>;

  constructor(client: LanguageClient, type: ProtocolNotificationType<P, TextDocumentRegistrationOptions>) {
    super(client);
    this._type = type;
  }

  protected abstract createParams(params: A): P;

  protected abstract runWith(params: P): R;

  public run(params: A): R {
    return this.runWith(this.createParams(params));
  }

}
