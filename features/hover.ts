import { ClientCapabilities, RegistrationType, HoverParams, HoverRegistrationOptions, HoverRequest, MarkupKind, Hover, MarkupContent, MarkedString } from "vscode-languageserver-protocol";
import { LanguageClient } from "../client";
import { RunnableDynamicFeature, ensure } from "./features";

export class HoverFeature extends RunnableDynamicFeature<HoverParams, HoverParams, Promise<string | null>, HoverRegistrationOptions> {

  private readonly markup = '```';

  constructor(private client: LanguageClient) {
    super();
  }

  public fillClientCapabilities(capabilities: ClientCapabilities): void {
    const hoverCapability = (ensure(ensure(capabilities, 'textDocument')!, 'hover')!);
    hoverCapability.dynamicRegistration = true;
    hoverCapability.contentFormat = [MarkupKind.Markdown, MarkupKind.PlainText];
  }

  public async runWith(params: HoverParams): Promise<string | null> {
    const resp = await this.client.sendRequest(HoverRequest.type, params);
    if (resp == null) return null;

    return this.parseHoverContens(resp, []);
  }

  private parseHoverContens(hover: Hover, result: string[]) {
    const { contents } = hover;
    if (MarkedString.is(contents)) {
      if (typeof contents == 'string') {
        if (contents.startsWith('```'))
          result.push(contents.trim());
        else
          result.push(this.codeBlockFor('text', contents.trim()));
      } else {
        result.push(this.codeBlockFor(contents.language, contents.value.trim()));
      }
    } else if (MarkupContent.is(contents)) {
      result.push(
        contents.kind == MarkupKind.Markdown
          ? contents.value.trim()
          : this.codeBlockFor('text', contents.value.trim())
      );
    } else {
      for (const it of contents) {
        this.parseHoverContens(it as unknown as Hover, result);
      }
    }

    return result.join('\n');
  }

  private codeBlockFor(language: string, content: string) {
    return `${this.markup} ${language}
    ${content}
    ${this.markup}`;
  }

  public get registrationType(): RegistrationType<HoverRegistrationOptions> {
    return HoverRequest.type;
  }

}