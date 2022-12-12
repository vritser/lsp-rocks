import { ClientCapabilities, ServerCapabilities, DocumentSelector, RegistrationType, DefinitionRegistrationOptions, Location, DocumentUri, Position, DeclarationRequest, DeclarationParams, DeclarationRegistrationOptions } from "vscode-languageserver-protocol";
import { LanguageClient } from "../client";
import { RunnableDynamicFeature, ensure } from "./features";
import { fileURLToPath } from 'node:url'

interface EmacsDefinitionResp {
  uri: DocumentUri,
  position: Position,
}

export class DeclarationFeature extends RunnableDynamicFeature<DeclarationParams, DeclarationParams, Promise<EmacsDefinitionResp>, DefinitionRegistrationOptions> {

  constructor(private client: LanguageClient) {
    super();
  }

  public fillClientCapabilities(capabilities: ClientCapabilities): void {
    const definitionSupport = ensure(ensure(capabilities, 'textDocument')!, 'declaration')!;
    definitionSupport.dynamicRegistration = true;
    definitionSupport.linkSupport = true;
  }

  public initialize(capabilities: ServerCapabilities<any>, documentSelector: DocumentSelector | undefined): void {
    //
  }

  public createParams(params: DeclarationParams): DeclarationParams {
    return params;
  }

  public async runWith(params: DeclarationParams): Promise<EmacsDefinitionResp> {
    const emptyDefinition = { uri: '', position: { line: 0, character: 0 } };
    const resp = await this.client.sendRequest(DeclarationRequest.type, params);
    if (resp == null) return emptyDefinition;

    if (Array.isArray(resp)) {
      if (resp.length > 0) {
        const [location] = resp;
        if (this.isLocation(location)) {
          return {
            uri: fileURLToPath(location.uri),
            position: location.range.start,
          };
        } else {
          return {
            uri: fileURLToPath(location.targetUri),
            position: location.targetRange.start,
          };
        }
      }
      return emptyDefinition;
    } else {
      return {
        uri: fileURLToPath(resp.uri),
        position: resp.range.start,
      };
    }
  }

  private isLocation(value: any): value is Location {
    return 'uri' in value && 'range' in value;
  }

  public get registrationType(): RegistrationType<DeclarationRegistrationOptions> {
    return DeclarationRequest.type;
  }

}