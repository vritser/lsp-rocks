import { ClientCapabilities, RegistrationType, HoverParams, HoverRegistrationOptions, HoverRequest, MarkupKind, Hover, MarkupContent, MarkedString } from "vscode-languageserver-protocol";
import { LanguageClient } from "../client";
import { RunnableDynamicFeature, ensure } from "./features";

export class HoverFeature extends RunnableDynamicFeature<HoverParams, HoverParams, Promise<string | null>, HoverRegistrationOptions> {

  private readonly markup = '```';

  constructor(private client: LanguageClient) {
    super();
  }

  public fillClientCapabilities(capabilities: ClientCapabilities): void {
    const hoverSupport = (ensure(ensure(capabilities, 'textDocument')!, 'hover')!);
    hoverSupport.dynamicRegistration = true;
    hoverSupport.contentFormat = [MarkupKind.Markdown, MarkupKind.PlainText];
  }

  public async runWith(params: HoverParams): Promise<string | null> {
    const resp = await this.client.sendRequest(HoverRequest.type, params);
    if (resp == null) return null;

    return this.parseHoverContens(resp.contents, []);
  }

  private parseHoverContens(contents: MarkupContent | MarkedString | MarkedString[], result: string[]) {
    if (MarkedString.is(contents)) {
      if (typeof contents == 'string') {
        if (contents.startsWith('```'))
          result.push(contents);
        else
          result.push(contents);
      } else {
        result.push(this.codeBlockFor(contents.language, contents.value));
      }
    } else if (MarkupContent.is(contents)) {
      result.push(
        contents.kind == MarkupKind.Markdown
          ? contents.value.trimStart()
          : this.codeBlockFor('text', contents.value)
      );
    } else {
      result.push(this.parseMarkedStringArray(contents));
    }

    return result.join('\n');
  }

  private parseMarkedStringArray(contents: MarkedString[]) {
    const ret: string[] = [];
    for (const it of contents) {
      if (typeof it == 'string') {
        ret.push(it);
      } else {
        ret.push(this.codeBlockFor(it.language, it.value));
        ret.push('___');
      }
    }
    return ret.join('\n');
  }

  /**
   * Make a markdown code block
   * @param language language of the code block
   * @param content content of the code block
   * @returns a code block string
   */
  private codeBlockFor(language: string, content: string) {
    return `${this.markup} ${language}
    ${content}
    ${this.markup}`;
  }

  public get registrationType(): RegistrationType<HoverRegistrationOptions> {
    return HoverRequest.type;
  }

}