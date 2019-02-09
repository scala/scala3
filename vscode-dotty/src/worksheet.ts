import * as vscode from 'vscode'
import {
  CancellationToken, CancellationTokenSource, CodeLens, CodeLensProvider, Command,
  Event, EventEmitter, ProgressLocation, Range, TextDocument, TextEdit
} from 'vscode'

import {
  asWorksheetRunParams, WorksheetRunRequest, WorksheetRunResult,
  WorksheetPublishOutputParams, WorksheetPublishOutputNotification
} from './protocol'
import { BaseLanguageClient } from 'vscode-languageclient'
import { Disposable } from 'vscode-jsonrpc'

/**
 * The command key for running a worksheet. Exposed to users as
 * `Run worksheet`.
 */
export const worksheetRunKey = "dotty.worksheet.run"

/**
 * The command key for cancelling a running worksheet. Exposed to users as
 * `Cancel running worksheet`.
 */
export const worksheetCancelKey = "dotty.worksheet.cancel"

/**
 * If true, the setting for running the worksheet on save is enabled.
 */
function runWorksheetOnSave(): boolean {
  return vscode.workspace.getConfiguration("dotty").get("runWorksheetOnSave") as boolean
}

/**
 * A wrapper around the information that VSCode needs to display text decorations.
 *
 * @param decorationType    The styling options of this decoration
 * @param decorationOptions The options of this decoraiton.
 */
class Decoration {
  constructor(readonly decorationType: vscode.TextEditorDecorationType,
              readonly decorationOptions: vscode.DecorationOptions) {
  }
}

/** A worksheet managed by vscode */
class Worksheet implements Disposable {

  /** The version of this document the last time it was run */
  private runVersion: number = -1

  /** All decorations that have been added so far */
  private decorations: Decoration[] = []

  /** The minimum margin to add so that the decoration is shown after all text. */
  private margin: number = 0

  private readonly _onDidStateChange: EventEmitter<void> = new EventEmitter()
  /** This event is fired when the worksheet starts or stops running. */
  readonly onDidStateChange: Event<void> = this._onDidStateChange.event

  /**
   * If this is not null, this can be used to signal cancellation of the
   * currently running worksheet.
   */
  private canceller?: CancellationTokenSource = undefined

  constructor(readonly document: vscode.TextDocument, readonly client: BaseLanguageClient) {
  }

	dispose() {
    this.reset()
    this._onDidStateChange.dispose()
  }

  /** Cancel any current run, remove all decorations, and resets this worksheet. */
  private reset(): void {
    this.cancel()

    this.decorations.forEach(decoration => decoration.decorationType.dispose())
    this.decorations = []
    this.runVersion = -1
    this.margin = this.longestLine() + 5
  }

  /** If this worksheet is currently being run, cancel the run. */
  cancel(): void {
    if (this.canceller) {
      this.canceller.cancel()
      this.canceller = undefined

      this._onDidStateChange.fire()
    }
  }

  /** Is this worksheet currently being run ? */
  isRunning(): boolean {
    return this.canceller != undefined
  }

  /** Display the output in the worksheet's editor. */
  handleMessage(output: WorksheetPublishOutputParams, editor: vscode.TextEditor) {
    let range = output.range ?
      this.client.protocol2CodeConverter.asRange(output.range) :
      (output.line ?
        new Range(output.line - 1, 0, output.line - 1, 0) :
        new Range(0, 0, 0, 0))
    this.displayAndSaveResult(range, output.content, editor)
  }

  /**
   * Run the worksheet in `document`, if a previous run is in progress, it is
   * cancelled first.
   */
  run(): Promise<WorksheetRunResult> {
    this.cancel()
    this.reset()
    const canceller = new CancellationTokenSource()
    const token = canceller.token
    this.canceller = canceller  // This ensures that isRunning() returns true.

    this._onDidStateChange.fire()

    return new Promise<WorksheetRunResult>(resolve => {
      this.runVersion = this.document.version
      resolve(
        vscode.window.withProgress({
          location: ProgressLocation.Window,
          title: "Running worksheet"
        }, () => this.client.sendRequest(
          WorksheetRunRequest.type, asWorksheetRunParams(this.document), token
        )))
    }).then(result => {
      canceller.dispose()
      if (this.canceller === canceller) { // If false, a new run has already started
        // This ensures that isRunning() returns false.
        this.canceller = undefined

        this._onDidStateChange.fire()
      }
      return result
    })
  }

  /**
   * Parse and display the result of running part of this worksheet. The result is saved so that it
   * can be restored if this buffer is closed.
   *
   * @param range      The range in the source that produced the result.
   * @param runResult  The result itself.
   * @param editor     The editor where to display the result.
   */
  private displayAndSaveResult(range: Range, runResult: string, editor: vscode.TextEditor): void {
    const resultLines = runResult.split(/\r\n|\r|\n/g)

    if (resultLines.length == 0)
      return

    const lastLine = editor.document.lineAt(range.end.line)
    const decorationOptions = {
      range: range,
      hoverMessage: new vscode.MarkdownString().appendCodeblock(runResult)
    }
    const decorationMargin = this.margin - lastLine.text.length
    const decorationText = resultLines[0] + (resultLines.length > 1 ? `<${resultLines.length - 1} lines hidden, hover to see full output>` : "")
    const decorationType = this.createDecoration(decorationMargin, decorationText)
    const decoration = new Decoration(decorationType, decorationOptions)
    this.decorations.push(decoration)
    editor.setDecorations(decorationType, [decorationOptions])
  }

  /**
   * Restore the decorations that belong to this worksheet in `editor`. If the document has been
   * changed, since the last run of the worksheet, the decorations won't be added.
   *
   * @param editor The editor where to display the decorations.
   */
  restoreDecorations(editor: vscode.TextEditor) {
    if (editor.document.version == this.runVersion) {
      this.decorations.forEach(decoration => {
        editor.setDecorations(decoration.decorationType, [decoration.decorationOptions])
      })
    }
  }

  /**
   * Create a new `TextEditorDecorationType` showing `text`. The decoration
   * will appear `margin` characters after the end of the line.
   *
   * @param margin The margin in characters between the end of the line
   *               and the decoration.
   * @param text   The text of the decoration.
   * @return a new `TextEditorDecorationType`.
   */
  private createDecoration(margin: number, text: string) {
    return vscode.window.createTextEditorDecorationType({
      isWholeLine: true,
      after: {
        contentText: text,
        margin: `0px 0px 0px ${margin}ch`,
        fontStyle: "italic",
        // It would make more sense to use the colors of commments in the
        // current theme, but there's no API to access this currently
        // (https://github.com/Microsoft/vscode/issues/32813).
        color: new vscode.ThemeColor("terminal.ansiGreen"),
      }
    })
  }

  /**
   * Finds the length in characters of the longest line of `document`.
   *
   * @param document The document to inspect.
   * @return The length in characters of the longest line.
   */
  private longestLine() {
    let maxLength = 0
    const lineCount = this.document.lineCount
    for (let i = 0; i < lineCount; ++i) {
      let length = this.document.lineAt(i).text.length
      maxLength = Math.max(maxLength, length)
    }

    return maxLength
  }
}

export class WorksheetProvider implements Disposable {
  private worksheets: Map<vscode.TextDocument, Worksheet> = new Map()
  private readonly _onDidWorksheetStateChange: EventEmitter<Worksheet> = new EventEmitter()
  /** This event is fired when a worksheet starts or stops running. */
  readonly onDidWorksheetStateChange: Event<Worksheet> = this._onDidWorksheetStateChange.event

  private disposables: Disposable[] = [ this._onDidWorksheetStateChange ]

  constructor(
      readonly client: BaseLanguageClient,
      readonly documentSelector: vscode.DocumentSelector) {
    const codeLensProvider = new WorksheetCodeLensProvider(this)
    this.disposables.push(
      codeLensProvider,
      vscode.languages.registerCodeLensProvider(documentSelector, codeLensProvider),
      vscode.workspace.onWillSaveTextDocument(event => {
        const document = event.document
        const worksheet = this.worksheetFor(document)
        // If the document is not dirty, then `onDidSaveTextDocument` will not
        // be called so we need to run the worksheet now.
        // On the other hand, if the document _is_ dirty, we should _not_ run
        // the worksheet now because the server state will not be synchronized
        // with the client state, instead we let `onDidSaveTextDocument`
        // handle it.
        if (worksheet && runWorksheetOnSave() && !document.isDirty) {
          worksheet.run()
        }
      }),
      vscode.workspace.onDidSaveTextDocument(document => {
        const worksheet = this.worksheetFor(document)
        if (worksheet && runWorksheetOnSave()) {
          worksheet.run()
        }
      }),
      vscode.workspace.onDidCloseTextDocument(document => {
        const worksheet = this.worksheetFor(document)
        if (worksheet) {
          worksheet.dispose()
          this.worksheets.delete(document)
        }
      }),
      vscode.window.onDidChangeActiveTextEditor(editor => {
        if (editor) {
          const worksheet = this.worksheetFor(editor.document)
          if (worksheet) {
            worksheet.restoreDecorations(editor)
          }
        }
      }),
      vscode.commands.registerCommand(worksheetRunKey, () => {
        this.callOnActiveWorksheet(w => w.run())
      }),
      vscode.commands.registerCommand(worksheetCancelKey, () => {
        this.callOnActiveWorksheet(w => w.cancel())
      })
    )
    client.onNotification(WorksheetPublishOutputNotification.type, params => {
      this.handleMessage(params)
    })
  }

	dispose() {
    this.worksheets.forEach(d => d.dispose())
    this.worksheets.clear()
		this.disposables.forEach(d => d.dispose())
		this.disposables = []
	}

  /** Is this document a worksheet? */
  private isWorksheet(document: vscode.TextDocument): boolean {
    return vscode.languages.match(this.documentSelector, document) > 0
  }

  /** If `document` is a worksheet, create a new worksheet for it, or return the existing one. */
  worksheetFor(document: vscode.TextDocument): Worksheet | undefined {
    if (!this.isWorksheet(document)) return
    else {
      const existing = this.worksheets.get(document)
      if (existing) {
        return existing
      } else {
        const newWorksheet = new Worksheet(document, this.client)
        this.worksheets.set(document, newWorksheet)
        this.disposables.push(
          newWorksheet.onDidStateChange(() => this._onDidWorksheetStateChange.fire(newWorksheet))
        )
        return newWorksheet
      }
    }
  }

  /** If the active text editor contains a worksheet, apply `f` to it. */
  private callOnActiveWorksheet(f: (_: Worksheet) => void) {
    let document = vscode.window.activeTextEditor && vscode.window.activeTextEditor.document
    if (document) {
      const worksheet = this.worksheetFor(document)
      if (worksheet) {
        f(worksheet)
      }
    }
  }

  /**
   * Handle the result of running part of a worksheet.
   * This is called when we receive a `worksheet/publishOutput`.
   *
   * @param output The result of running part of a worksheet.
   */
  private handleMessage(output: WorksheetPublishOutputParams) {
    const editor = vscode.window.visibleTextEditors.find(e => {
      let uri = e.document.uri.toString()
      return uri == output.textDocument.uri
    })

    if (editor) {
      const worksheet = this.worksheetFor(editor.document)
      if (worksheet) {
        worksheet.handleMessage(output, editor)
      }
    }
  }
}

class WorksheetCodeLensProvider implements CodeLensProvider, Disposable {
  private readonly _onDidChangeCodeLenses: EventEmitter<void> = new EventEmitter()
  readonly onDidChangeCodeLenses: Event<void> = this._onDidChangeCodeLenses.event

  private disposables: Disposable[] = [ this._onDidChangeCodeLenses ]

  constructor(readonly worksheetProvider: WorksheetProvider) {
    this.disposables.push(
      worksheetProvider.onDidWorksheetStateChange(() => this._onDidChangeCodeLenses.fire())
    )
  }

  dispose() {
    this.disposables.forEach(d => d.dispose())
    this.disposables = []
  }

  private readonly runCommand: Command = {
    command: worksheetRunKey,
    title: "Run this worksheet"
  }

  private readonly cancelCommand: Command = {
    command: worksheetCancelKey,
    title: "Worksheet running, click to cancel"
  }

  provideCodeLenses(document: TextDocument, token: CancellationToken) {
    const worksheet = this.worksheetProvider.worksheetFor(document)
    if (worksheet) {
      const cmd = worksheet.isRunning() ? this.cancelCommand : this.runCommand
      return [ new CodeLens(new Range(0, 0, 0, 0), cmd) ]
    }
  }
}
