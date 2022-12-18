import { ClientCapabilities, RegistrationType, MarkupKind, SignatureHelpParams, SignatureHelpRegistrationOptions, SignatureHelpRequest, SignatureHelp, MarkupContent } from "vscode-languageserver-protocol";
import { LanguageClient } from "../client";
import { RunnableDynamicFeature, ensure } from "./features";

export class SignatureHelpFeature extends RunnableDynamicFeature<SignatureHelpParams, SignatureHelpParams, Promise<SignatureHelp | null>, SignatureHelpRegistrationOptions> {

  constructor(private client: LanguageClient) {
    super();
  }

  public fillClientCapabilities(capabilities: ClientCapabilities): void {
    const signatureHelpSupport = ensure(ensure(capabilities, 'textDocument')!, 'signatureHelp')!;
    signatureHelpSupport.dynamicRegistration = true;
    signatureHelpSupport.signatureInformation = { documentationFormat: [MarkupKind.Markdown, MarkupKind.PlainText] };
    signatureHelpSupport.signatureInformation.parameterInformation = { labelOffsetSupport: true };
    signatureHelpSupport.signatureInformation.activeParameterSupport = true;
    signatureHelpSupport.contextSupport = true;
  }

  public async runWith(params: SignatureHelpParams): Promise<SignatureHelp | null> {
    const resp = await this.client.sendRequest(SignatureHelpRequest.type, params);
    if (resp == null) return null;

    const { activeParameter, activeSignature, signatures } = resp;
    const current = signatures[activeSignature || 0];

    if (current.parameters != undefined && current.parameters.length > 0) {
      const paramIdx = current.activeParameter || activeParameter || 0;
      const param = current.parameters[paramIdx];
      if (typeof param.label == 'string') {
        current.label = current.label.replace(param.label, `*${param.label}*`);
      } else {
        const [start, end] = param.label;
        const label = current.label.slice(start, end);
        current.label = current.label.replace(label, `*${label}*`);
      }

      if (current.documentation != undefined) {
        const { documentation } = current;
        if (MarkupContent.is(documentation)) {
          if (documentation.kind == MarkupKind.Markdown) {
            current.documentation = documentation.value;
          }
        }
      }
    }

    return resp;
  }

  public get registrationType(): RegistrationType<SignatureHelpRegistrationOptions> {
    return SignatureHelpRequest.type;
  }

}
