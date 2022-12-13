import { ClientCapabilities, RegistrationType, DefinitionRegistrationOptions, Location, DeclarationRequest, DeclarationParams, DeclarationRegistrationOptions, LocationLink } from "vscode-languageserver-protocol";
import { LanguageClient } from "../client";
import { RunnableDynamicFeature, ensure } from "./features";
import { fileURLToPath } from 'node:url'

export class DeclarationFeature extends RunnableDynamicFeature<DeclarationParams, DeclarationParams, Promise<Location[]>, DefinitionRegistrationOptions> {

  constructor(private client: LanguageClient) {
    super();
  }

  public fillClientCapabilities(capabilities: ClientCapabilities): void {
    const definitionSupport = ensure(ensure(capabilities, 'textDocument')!, 'declaration')!;
    definitionSupport.dynamicRegistration = true;
    definitionSupport.linkSupport = true;
  }

  public async runWith(params: DeclarationParams): Promise<Location[]> {
    const resp = await this.client.sendRequest(DeclarationRequest.type, params);
    if (resp == null) return [];

    if (Array.isArray(resp)) {
      return resp.map((it: Location | LocationLink) => {
        if (this.isLocation(it)) {
          return { uri: fileURLToPath(it.uri), range: it.range };
        } else {
          return { uri: fileURLToPath(it.targetUri), range: it.targetRange };
        }
      });
    }

    return [{ uri: fileURLToPath(resp.uri), range: resp.range }];
  }

  private isLocation(value: any): value is Location {
    return 'uri' in value && 'range' in value;
  }

  public get registrationType(): RegistrationType<DeclarationRegistrationOptions> {
    return DeclarationRequest.type;
  }

}