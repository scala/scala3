import * as vscode from 'vscode'

import {
  asWorksheetRunParams, WorksheetRunRequest, WorksheetRunParams,
  WorksheetPublishOutputParams, WorksheetPublishOutputNotification
} from './protocol'
import { BaseLanguageClient, DocumentSelector } from 'vscode-languageclient'
import { Disposable } from 'vscode-jsonrpc'

/**
 * The command key for running a worksheet. Exposed to users as
 * `Run worksheet`.
 */
export const worksheetRunKey = "dotty.worksheet.run"

/** A worksheet managed by vscode */
class Worksheet {

  constructor(readonly document: vscode.TextDocument, readonly client: BaseLanguageClient) {
  }

  /** All decorations that have been added so far */
  private decorationTypes: vscode.TextEditorDecorationType[] = []

  /** The number of blank lines that have been inserted to fit the output so far. */
  private insertedLines: number = 0

  /** The lines that contain decorations */
  private decoratedLines: Set<number> = new Set<number>()

  /** The minimum margin to add so that the decoration is shown after all text. */
  private margin: number = 0

  /** Remove all decorations and resets this worksheet. */
  private reset(): void {
    this.decorationTypes.forEach(decoration => decoration.dispose())
    this.insertedLines = 0
    this.decoratedLines.clear()
    this.margin = this.longestLine() + 5
  }

  /**
   * Reset the "worksheet state" (margin and number of inserted lines), and
   * removes redundant blank lines that have been inserted by a previous
   * run.
   */
  prepareForRunning(): void {
    this.removeRedundantBlankLines().then(_ => this.reset())
  }

  /**
   * Run the worksheet in `document`, display a progress bar during the run.
   */
  run(): Thenable<{}> {
    return vscode.window.withProgress({
      location: vscode.ProgressLocation.Notification,
      title: "Run the worksheet",
      cancellable: true
    }, (_, token) => {
      return this.client.sendRequest(WorksheetRunRequest.type, asWorksheetRunParams(this.document), token)
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
   * Remove the repeated blank lines in the source.
   *
   * Running a worksheet can insert new lines in the worksheet so that the
   * output of a line fits below the line. Before a run, we remove blank
   * lines in the worksheet to keep its length under control.
   *
   * @param worksheet The worksheet where blank lines must be removed.
   * @return A `Thenable` removing the blank lines upon completion.
   */
  private removeRedundantBlankLines() {

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

    return rangesToRemove.reverse().reduce((chain: Thenable<boolean>, range) => {
      return chain.then(_ => {
        const edit = new vscode.WorkspaceEdit()
        edit.delete(document.uri, range)
        return vscode.workspace.applyEdit(edit)
      })
    }, Promise.resolve(true))
  }

  private hasDecoration(line: number): boolean {
    return this.decoratedLines.has(line)
  }
}

export class WorksheetProvider implements Disposable {
  private disposables: Disposable[] = []
  private worksheets: Map<vscode.TextDocument, Worksheet> = new Map()

  constructor(
      readonly client: BaseLanguageClient,
      readonly documentSelectors: vscode.DocumentSelector[]) {
    this.disposables.push(
      vscode.workspace.onWillSaveTextDocument(event => {
        const worksheet = this.worksheetFor(event.document)
        if (worksheet) {
          // Block file saving until the worksheet is ready to be run.
          worksheet.prepareForRunning()
        }
      }),
      vscode.workspace.onDidSaveTextDocument(document => {
        const worksheet = this.worksheetFor(document)
        if (worksheet) {
          return worksheet.run()
        }
      }),
      vscode.workspace.onDidCloseTextDocument(document => {
        if (this.isWorksheet(document)) {
          this.worksheets.delete(document)
        }
      }),
      vscode.commands.registerCommand(worksheetRunKey, () => {
        this.runWorksheetCommand()
      })
    )
    client.onNotification(WorksheetPublishOutputNotification.type, params => {
      this.handleMessage(params)
    })
  }

	dispose() {
		this.disposables.forEach(d => d.dispose());
		this.disposables = [];
	}

  /** Is this document a worksheet? */
  private isWorksheet(document: vscode.TextDocument): boolean {
    return this.documentSelectors.some(sel => vscode.languages.match(sel, document) > 0)
  }

  /** If `document` is a worksheet, create a new worksheet for it, or return the existing one. */
  private worksheetFor(document: vscode.TextDocument): Worksheet | undefined {
    if (!this.isWorksheet(document)) return
    else {
      const existing = this.worksheets.get(document)
      if (existing) {
        return existing
      } else {
        const newWorksheet = new Worksheet(document, this.client)
        this.worksheets.set(document, newWorksheet)
        return newWorksheet
      }
    }
  }

  /**
   * The VSCode command executed when the user select `Run worksheet`.
   *
   * We check whether the buffer is dirty, and if it is, we save it. Running the worksheet will then be
   * triggered by file save.
   * If the buffer is clean, we do the necessary preparation for worksheet (compute margin,
   * remove blank lines, etc.) and check if the buffer has been changed by that. If it is, we save
   * and the run will be triggered by file save.
   * If the buffer is still clean, call `Worksheet#run`.
   */
  private runWorksheetCommand() {
    const editor = vscode.window.activeTextEditor
    if (editor) {
      const document = editor.document
      const worksheet = this.worksheetFor(document)
      if (worksheet) {
        if (document.isDirty) document.save() // This will trigger running the worksheet
        else {
          worksheet.prepareForRunning()
          if (document.isDirty) document.save() // This will trigger running the worksheet
          else {
            worksheet.run()
          }
        }
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
