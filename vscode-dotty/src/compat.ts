import * as vscode from 'vscode'

import { HoverRequest } from 'vscode-languageclient'
import { MarkedString, LanguageClient, LanguageClientOptions, RevealOutputChannelOn,
         ServerOptions } from 'vscode-languageclient'

// Fix hover functionality when using this version of vscode-dotty with Dotty
// Language Server 0.9 or earlier.
// Without this, the displayed hover would be "[object Object]".
export function enableOldServerWorkaround(client: LanguageClient): void {
  client.clientOptions.middleware = {
    provideHover: (document, position, token, next) => {
      // This code is adapted from HoverFeature#registerLanguageProvider in
      // https://github.com/Microsoft/vscode-languageserver-node/blob/master/client/src/client.ts
      return client
        .sendRequest(HoverRequest.type,
                     client.code2ProtocolConverter.asTextDocumentPositionParams(document, position), token)
        .then(
          hover => {
            if (!hover) {
              return undefined
            }
            // The server is supposed to return string or { language: string,
            // value: string } or an array of those things, but instead it returns
            // an array of { value: string }. This used to work but with a recent
            // vscode-languageclient the user ends up seeing "[object Object]" as
            // the hover information.
            // We work around this by manually parsing the array of { value: string }
            // into a Hover.
            const contents = hover.contents as { value: string }[]
            let result: vscode.MarkdownString[] = []
            for (let element of contents) {
              let item = new vscode.MarkdownString()
              item.appendCodeblock(element.value, "scala")
              result.push(item)
            }
            return new vscode.Hover(result)
          },
          error => {
            client.logFailedRequest(HoverRequest.type, error)
            return Promise.resolve(null)
          }
        )
    }
  }
}
