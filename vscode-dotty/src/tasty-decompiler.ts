import * as vscode from 'vscode'
import * as path from 'path'
import { CancellationTokenSource, ProgressLocation } from 'vscode'
import { TastyDecompileRequest, TastyDecompileResult,
         asTastyDecompileParams, } from './protocol'
import { BaseLanguageClient } from 'vscode-languageclient'
import { Disposable } from 'vscode-jsonrpc'

const RESULT_OK = 0
const ERROR_TASTY_VERSION = 1
const ERROR_CLASS_NOT_FOUND = 2
const ERROR_OTHER = -1

export class TastyDecompilerProvider implements Disposable {
  private disposables: Disposable[] = []

  constructor(
    readonly client: BaseLanguageClient,
    readonly documentSelector: vscode.DocumentSelector,
    readonly provider: DecompiledDocumentProvider) {
      this.disposables.push(
        vscode.workspace.onDidOpenTextDocument(textDocument => {
          if (this.isTasty(textDocument)) {
            this.requestDecompile(textDocument).then(decompileResult => {
              switch (decompileResult.error) {
                case RESULT_OK:
                  let scalaDocument = provider.makeScalaDocument(textDocument, decompileResult.scala)

                  vscode.workspace.openTextDocument(scalaDocument).then(doc => {
                    vscode.window.showTextDocument(doc, 1)
                  })

                  let fileName = textDocument.fileName.substring(textDocument.fileName.lastIndexOf(path.sep) + 1)

                  TastyTreeView.create(fileName, decompileResult.tastyTree)
                  break
                case ERROR_TASTY_VERSION:
                  vscode.window.showErrorMessage("Tasty file has unexpected signature.")
                  break
                case ERROR_CLASS_NOT_FOUND:
                  vscode.window.showErrorMessage("The class file related to this TASTy file could not be found.")
                  break
                case ERROR_OTHER:
                  vscode.window.showErrorMessage("A decompilation error has occurred.")
                  break
                default:
                  vscode.window.showErrorMessage("Unknown Error.")
                  break
              }
            })
          }
        })
      )
  }

  dispose(): void {
    this.disposables.forEach(d => d.dispose())
    this.disposables = []
  }

  /**
   * Request the TASTy in `textDocument` to be decompiled
   */
  private requestDecompile(textDocument: vscode.TextDocument): Promise<TastyDecompileResult> {
    const requestParams = asTastyDecompileParams(textDocument)
    const canceller = new CancellationTokenSource()
    const token = canceller.token

    return new Promise<TastyDecompileResult>(resolve => {
      resolve(vscode.window.withProgress({
        location: ProgressLocation.Notification,
        title: "Decompiling"
      }, () => this.client.sendRequest(TastyDecompileRequest.type, requestParams, token)
      ))
    }).then(decompileResult => {
      canceller.dispose()
      return decompileResult
    })
  }

  /** Is this document a tasty file? */
  private isTasty(document: vscode.TextDocument): boolean {
    return vscode.languages.match(this.documentSelector, document) > 0
  }
}

/**
 * Provider of virtual, read-only, scala documents
 */
export class DecompiledDocumentProvider implements vscode.TextDocumentContentProvider {
  static scheme = 'decompiled'

  private _documents = new Map<string, string>()
  private _subscriptions: vscode.Disposable

  constructor() {
    // Don't keep closed documents in memory
    this._subscriptions = vscode.workspace.onDidCloseTextDocument(doc => this._documents.delete(doc.uri.toString()))
  }

  dispose() {
    this._subscriptions.dispose()
    this._documents.clear()
  }

  provideTextDocumentContent(uri: vscode.Uri): string {
    let document = this._documents.get(uri.toString())
    if (document) {
      return document
    } else {
      return 'Failed to load result.'
    }
  }

  /**
   * Creates a new virtual document ready to be provided and opened.
   *
   * @param textDocument  The document containing the TASTy that was decompiled
   * @param content       The source code provided by the language server
   */
  makeScalaDocument(textDocument: vscode.TextDocument, content: string): vscode.Uri {
    let scalaDocument = textDocument.uri.with({
      scheme: DecompiledDocumentProvider.scheme,
      path: textDocument.uri.path.replace(".tasty", ".scala")
    })
    this._documents.set(scalaDocument.toString(), content)
    return scalaDocument
  }
}

/**
 * WebView used as container for preformatted TASTy trees
 */
class TastyTreeView {
  public static readonly viewType = 'tastyTree'

  private readonly _panel: vscode.WebviewPanel
  private _disposables: vscode.Disposable[] = []

  /**
   * Create new panel for a TASTy tree in a new column or column 2 if none is currently open
   *
   * @param title     The panel's title
   * @param content   The panel's preformatted content
   */
  public static create(title: string, content: string) {
    const column = vscode.window.activeTextEditor ? vscode.window.activeTextEditor.viewColumn : undefined

    const panel = vscode.window.createWebviewPanel(TastyTreeView.viewType, "Tasty Tree", (column || vscode.ViewColumn.One) + 1, {})

    new TastyTreeView(panel, title, content)
  }

  private constructor(
    panel: vscode.WebviewPanel,
    title: string,
    content: string
  ) {
    this._panel = panel
    this.setContent(title, content)

    // Listen for when the panel is disposed
    // This happens when the user closes the panel or when the panel is closed programmatically
    this._panel.onDidDispose(() => this.dispose(), null, this._disposables)
  }

  public dispose() {
    this._panel.dispose()

    while (this._disposables.length) {
      const x = this._disposables.pop()
      if (x) {
        x.dispose()
      }
    }
  }

  private setContent(name: string, content: string) {
    this._panel.title = name
    this._panel.webview.html = this._getHtmlForWebview(content)
  }

  private _getHtmlForWebview(content: string) {
    return `<!DOCTYPE html>
            <html lang="en">
            <head>
                <style>
                pre {
                  font-family: Consolas, "Andale Mono WT", "Andale Mono", "Lucida Console", "Lucida Sans Typewriter", "DejaVu Sans Mono", "Bitstream Vera Sans Mono", "Liberation Mono", "Nimbus Mono L", Monaco, "Courier New", Courier, monospace
                }
                span.name {
                  color:magenta;
                }
                span.tree {
                  color:yellow;
                }
                span.length {
                  color:cyan;
                }
                </style>
                <meta charset="UTF-8">
                <meta name="viewport" content="width=device-width, initial-scale=1.0">
                <title>Tasty Tree</title>
            </head>
            <body>
              <pre>
${content}</pre>
            </body>
            </html>`
  }
}
