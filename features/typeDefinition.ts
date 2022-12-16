import { ClientCapabilities, RegistrationType, DefinitionParams, DefinitionRegistrationOptions, DefinitionRequest, Location, LocationLink, TypeDefinitionParams, TypeDefinitionRegistrationOptions, TypeDefinitionRequest } from "vscode-languageserver-protocol";
import { LanguageClient } from "../client";
import { RunnableDynamicFeature, ensure } from "./features";
import { fileURLToPath } from 'node:url'

export class TypeDefinitionFeature extends RunnableDynamicFeature<TypeDefinitionParams, TypeDefinitionParams, Promise<Location[]>, TypeDefinitionRegistrationOptions> {

  constructor(private client: LanguageClient) {
    super();
  }

  public fillClientCapabilities(capabilities: ClientCapabilities): void {
    const typeDefinitionSupport = ensure(ensure(capabilities, 'textDocument')!, 'typeDefinition')!;
    typeDefinitionSupport.dynamicRegistration = true;
    typeDefinitionSupport.linkSupport = true;
  }

  public async runWith(params: TypeDefinitionParams): Promise<Location[]> {
    const resp = await this.client.sendRequest(DefinitionRequest.type, params);
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

  public get registrationType(): RegistrationType<TypeDefinitionRegistrationOptions> {
    return TypeDefinitionRequest.type;
  }

}