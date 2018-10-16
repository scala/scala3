import * as vscode from 'vscode'
import {
  CancellationToken, CancellationTokenSource, CodeLens, CodeLensProvider, Command,
  Event, EventEmitter, ProgressLocation, Range, TextDocument, TextEdit
} from 'vscode'

import {
  asWorksheetRunParams, WorksheetRunRequest, WorksheetRunParams, WorksheetRunResult,
  WorksheetPublishOutputParams, WorksheetPublishOutputNotification
} from './protocol'
import { BaseLanguageClient, DocumentSelector } from 'vscode-languageclient'
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

/** A worksheet managed by vscode */
class Worksheet implements Disposable {

  /** All decorations that have been added so far */
  private decorationTypes: vscode.TextEditorDecorationType[] = []

  /** The number of blank lines that have been inserted to fit the output so far. */
  private insertedLines: number = 0

  /** The lines that contain decorations */
  private decoratedLines: Set<number> = new Set<number>()

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
    if (this.canceller) {
      this.canceller.dispose()
      this.canceller = undefined
    }
    this._onDidStateChange.dispose()
	}

  /** Remove all decorations, and resets this worksheet. */
  private reset(): void {
    this.decorationTypes.forEach(decoration => decoration.dispose())
    this.insertedLines = 0
    this.decoratedLines.clear()
    this.margin = this.longestLine() + 5
  }

  /**
   * Reset the "worksheet state" (margin and number of inserted lines), and
   * return an array of TextEdit that remove the redundant blank lines that have
   * been inserted by a previous run.
   */
  prepareRun(): TextEdit[] {
    const edits = this.removeRedundantBlankLinesEdits()
    this.reset()
    return edits
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

  /**
   * Run the worksheet in `document`, if a previous run is in progress, it is
   * cancelled first.
   */
  run(): Promise<WorksheetRunResult> {
    this.cancel()
    const canceller = new CancellationTokenSource()
    const token = canceller.token
    // This ensures that isRunning() returns true.
    this.canceller = canceller

    this._onDidStateChange.fire()

    return new Promise<WorksheetRunResult>(resolve => {
      const textEdits = this.prepareRun()
      const edit = new vscode.WorkspaceEdit()
      edit.set(this.document.uri, textEdits)
      vscode.workspace.applyEdit(edit).then(editSucceeded => {
        if (editSucceeded && !token.isCancellationRequested)
          resolve(vscode.window.withProgress({
            location: ProgressLocation.Window,
            title: "Running worksheet"
          }, () => this.client.sendRequest(
            WorksheetRunRequest.type, asWorksheetRunParams(this.document), token
          )))
        else
          resolve({ success: false })
      })
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
   * Parse and display the result of running part of this worksheet.
   *
   * @param lineNumber The number of the line in the source that produced the result.
   * @param runResult  The result itself.
   * @param worksheet  The worksheet that receives the result.
   * @param editor     The editor where to display the result.
   * @return A `Thenable` that will insert necessary lines to fit the output
   *         and display the decorations upon completion.
   */
  public displayResult(lineNumber: number, runResult: string, editor: vscode.TextEditor) {
    const resultLines = runResult.trim().split(/\r\n|\r|\n/g)

    // The line where the next decoration should be put.
    // It's the number of the line that produced the output, plus the number
    // of lines that we've inserted so far.
    let actualLine = lineNumber + this.insertedLines

    // If the output has more than one line, we need to insert blank lines
    // below the line that produced the output to fit the output.
    const addNewLinesEdit = new vscode.WorkspaceEdit()
    if (resultLines.length > 1) {
      const linesToInsert = resultLines.length - 1
      const editPos = new vscode.Position(actualLine + 1, 0) // add after the line
      addNewLinesEdit.insert(editor.document.uri, editPos, "\n".repeat(linesToInsert))
      this.insertedLines += linesToInsert
    }

    return vscode.workspace.applyEdit(addNewLinesEdit).then(_ => {
      for (let line of resultLines) {
        const decorationPosition = new vscode.Position(actualLine, 0)
        const decorationMargin = this.margin - editor.document.lineAt(actualLine).text.length
        const decorationType = this.createDecoration(decorationMargin, line)
        this.decorationTypes.push(decorationType)
        this.decoratedLines.add(actualLine)

        const decoration = { range: new vscode.Range(decorationPosition, decorationPosition), hoverMessage: line }
        editor.setDecorations(decorationType, [decoration])
        actualLine += 1
      }
    })
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
        color: "light gray",
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

  /**
   * TextEdits to remove the repeated blank lines in the source.
   *
   * Running a worksheet can insert new lines in the worksheet so that the
   * output of a line fits below the line. Before a run, we remove blank
   * lines in the worksheet to keep its length under control.
   *
   * @param worksheet The worksheet where blank lines must be removed.
   * @return An array of `TextEdit` that remove the blank lines.
   */
  private removeRedundantBlankLinesEdits(): TextEdit[] {

    const document = this.document
    const lineCount = document.lineCount
    let rangesToRemove: vscode.Range[] = []
    let rangeStart = 0
    let rangeEnd = 0
    let inRange = true

    function addRange() {
      inRange = false
      if (rangeStart < rangeEnd) {
        rangesToRemove.push(new vscode.Range(rangeStart, 0, rangeEnd, 0))
      }
      return
    }

    for (let i = 0; i < lineCount; ++i) {
      const isEmpty = document.lineAt(i).isEmptyOrWhitespace && this.hasDecoration(i)
      if (inRange) {
        if (isEmpty) rangeEnd += 1
        else addRange()
      } else {
        if (isEmpty) {
          rangeStart = i
          rangeEnd = i + 1
          inRange = true
        }
      }
    }

    if (inRange) {
      rangeEnd = lineCount
      addRange()
    }

    return rangesToRemove.reverse().map(range => vscode.TextEdit.delete(range))
  }

  private hasDecoration(line: number): boolean {
    return this.decoratedLines.has(line)
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
        const worksheet = this.worksheetFor(event.document)
        if (worksheet) {
          event.waitUntil(Promise.resolve(worksheet.prepareRun()))
        }
      }),
      vscode.workspace.onDidSaveTextDocument(document => {
        const runWorksheetOnSave = vscode.workspace.getConfiguration("dotty").get("runWorksheetOnSave")
        const worksheet = this.worksheetFor(document)
        if (runWorksheetOnSave && worksheet) {
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
   * @param message The result of running part of a worksheet.
   */
  private handleMessage(output: WorksheetPublishOutputParams) {
    const editor = vscode.window.visibleTextEditors.find(e => {
      let uri = e.document.uri.toString()
      return uri == output.textDocument.uri
    })

    if (editor) {
      const worksheet = this.worksheetFor(editor.document)
      if (worksheet) {
        worksheet.displayResult(output.line - 1, output.content, editor)
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
