import { ClientCapabilities, ServerCapabilities, DocumentSelector, RegistrationType, DefinitionParams, DefinitionRegistrationOptions, DefinitionRequest, Location, DocumentUri, Position } from "vscode-languageserver-protocol";
import { LanguageClient } from "../client";
import { RunnableDynamicFeature, ensure } from "./features";
import { fileURLToPath } from 'node:url'

interface EmacsDefinitionResp {
  uri: DocumentUri,
  position: Position,
}

export class DefinitionFeature extends RunnableDynamicFeature<DefinitionParams, DefinitionParams, Promise<EmacsDefinitionResp>, DefinitionRegistrationOptions> {

  constructor(private client: LanguageClient) {
    super();
  }

  public fillClientCapabilities(capabilities: ClientCapabilities): void {
    const definitionSupport = ensure(ensure(capabilities, 'textDocument')!, 'definition')!;
    definitionSupport.dynamicRegistration = true;
    definitionSupport.linkSupport = true;
  }

  public initialize(capabilities: ServerCapabilities<any>, documentSelector: DocumentSelector | undefined): void {
    //
  }

  public createParams(params: DefinitionParams): DefinitionParams {
    return params;
  }

  public async runWith(params: DefinitionParams): Promise<EmacsDefinitionResp> {
    const emptyDefinition = { uri: '', position: { line: 0, character: 0 } };
    const resp = await this.client.sendRequest(DefinitionRequest.type, params);
    if (resp == null) return emptyDefinition;
    console.log('definition: ', resp);
    if (Array.isArray(resp)) {
      if (resp.length > 0) {
        const [location] = resp;
        if (this.isLocation(location)) {
          console.log('position: ', location.range);
          return {
            uri: fileURLToPath(location.uri),
            position: location.range.start,
          };
        } else {
          console.log('position: ', location.targetRange);
          return {
            uri: fileURLToPath(location.targetUri),
            position: location.targetRange.start,
          };
        }
      }
      return emptyDefinition;
    } else {
      console.log('position: ', resp.range);
      return {
        uri: fileURLToPath(resp.uri),
        position: resp.range.start,
      };
    }
  }

  private isLocation(value: any): value is Location {
    return 'uri' in value && 'range' in value;
  }

  public get registrationType(): RegistrationType<DefinitionRegistrationOptions> {
    return DefinitionRequest.type;
  }

}