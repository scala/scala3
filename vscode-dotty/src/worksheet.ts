import * as vscode from 'vscode'

import { client } from './extension'
import {
  asWorksheetExecParams, WorksheetExecRequest, WorksheetExecParams, WorksheetPublishOutputParams
} from './protocol'

/** A worksheet managed by vscode */
class Worksheet {

  private constructor(document: vscode.TextDocument) {
    this.document = document
  }

  /** The text document that this worksheet represents. */
  readonly document: vscode.TextDocument

  /** All decorations that have been added so far */
  decorationTypes: vscode.TextEditorDecorationType[] = []

  /** The number of blank lines that have been inserted to fit the output so far. */
  insertedLines: number = 0

  /** The lines that contain decorations */
  decoratedLines: Set<number> = new Set<number>()

  /** The minimum margin to add so that the decoration is shown after all text. */
  margin: number = 0

  /** Whether this worksheet has finished evaluating. */
  finished: boolean = false

  /** Remove all decorations and resets this worksheet. */
  reset() {
    this.decorationTypes.forEach(decoration => decoration.dispose())
    this.insertedLines = 0
    this.decoratedLines.clear()
    this.margin = longestLine(this.document) + 5
    this.finished = false
  }

  /** All the worksheets */
  private static worksheets: Map<vscode.TextDocument, Worksheet> = new Map<vscode.TextDocument, Worksheet>()

  /**
   * If `document` is a worksheet, create a new worksheet for it, or return the existing one. */
  static getOrNewWorksheet(document: vscode.TextDocument): Worksheet | undefined {
    if (!isWorksheet(document)) return
    else {
      const existing = Worksheet.worksheets.get(document)
      if (existing) {
        return existing
      } else {
        const newWorksheet = new Worksheet(document)
        Worksheet.worksheets.set(document, newWorksheet)
        return newWorksheet
      }
    }
  }

  /** If it exists, remove the worksheet representing `document`. */
  static delete(document: vscode.TextDocument) {
    Worksheet.worksheets.delete(document)
  }
}

/**
 * The command key for evaluating a worksheet. Exposed to users as
 * `Run worksheet`.
 */
export const worksheetEvaluateKey = "worksheet.evaluate"

/** Remove the worksheet corresponding to the given document. */
export function removeWorksheet(document: vscode.TextDocument) {
  Worksheet.delete(document)
}

/** Is this document a worksheet? */
export function isWorksheet(document: vscode.TextDocument): boolean {
  return document.fileName.endsWith(".sc")
}

/**
 * The VSCode command executed when the user select `Run worksheet`.
 *
 * We check whether the buffer is dirty, and if it is, we save it. Evaluation will then be
 * triggered by file save.
 * If the buffer is clean, we do the necessary preparation for worksheet (compute margin,
 * remove blank lines, etc.) and check if the buffer has been changed by that. If it is, we save
 * and the evaluation will be triggered by file save.
 * If the buffer is still clean, call `evaluateWorksheet`.
 */
export function evaluateWorksheetCommand() {
  const editor = vscode.window.activeTextEditor
  if (editor) {
    const document = editor.document

    if (document.isDirty) document.save() // This will trigger evaluation
    else {
      const worksheet = Worksheet.getOrNewWorksheet(document)
      if (worksheet) {
        _prepareWorksheet(worksheet).then(_ => {
          if (document.isDirty) document.save() // This will trigger evaluation
          else evaluateWorksheet(document)
        })
      }
    }
  }
}

/**
 * Evaluate the worksheet in `document`, display a progress bar during evaluation.
 */
export function evaluateWorksheet(document: vscode.TextDocument): Thenable<{}> {

  const worksheet = Worksheet.getOrNewWorksheet(document)
  if (worksheet) {
    return vscode.window.withProgress({
      location: vscode.ProgressLocation.Notification,
      title: "Evaluating worksheet",
      cancellable: true
    }, (_, token) => {
      return client.sendRequest(WorksheetExecRequest.type, asWorksheetExecParams(document), token)
    })
  } else {
    return Promise.reject()
  }
}

/**
 * If the document that will be saved is a worksheet, resets the "worksheet state"
 * (margin and number of inserted lines), and removes redundant blank lines that
 * have been inserted by a previous evaluation.
 *
 * The file save operation is blocked until the worksheet is ready to be evaluated.
 *
 * @param event `TextDocumentWillSaveEvent`.
 */
export function prepareWorksheet(event: vscode.TextDocumentWillSaveEvent) {
  const worksheet = Worksheet.getOrNewWorksheet(event.document)
  if (worksheet) {
    const setup = _prepareWorksheet(worksheet)
    event.waitUntil(setup)
  }
}

function _prepareWorksheet(worksheet: Worksheet) {
  return removeRedundantBlankLines(worksheet).then(_ => worksheet.reset())
}

/**
 * Handle the result of evaluating part of a worksheet.
 * This is called when we receive a `window/logMessage`.
 *
 * @param message The result of evaluating part of a worksheet.
 */
export function handleMessage(output: WorksheetPublishOutputParams) {

  const editor = vscode.window.visibleTextEditors.find(e => {
    let uri = e.document.uri.toString()
    return uri == output.textDocument.uri
  })

  if (editor) {
    const worksheet = Worksheet.getOrNewWorksheet(editor.document)

    if (worksheet) {
      worksheetDisplayResult(output.line - 1, output.content, worksheet, editor)
    }
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
function worksheetCreateDecoration(margin: number, text: string) {
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
function longestLine(document: vscode.TextDocument) {
  let maxLength = 0
  const lineCount = document.lineCount
  for (let i = 0; i < lineCount; ++i) {
    let length = document.lineAt(i).text.length
    maxLength = Math.max(maxLength, length)
  }

  return maxLength
}

/**
 * Remove the repeated blank lines in the source.
 *
 * Evaluating a worksheet can insert new lines in the worksheet so that the
 * output of a line fits below the line. Before evaluation, we remove blank
 * lines in the worksheet to keep its length under control.
 *
 * @param worksheet The worksheet where blank lines must be removed.
 * @return A `Thenable` removing the blank lines upon completion.
 */
function removeRedundantBlankLines(worksheet: Worksheet) {

  function hasDecoration(line: number): boolean {
    return worksheet.decoratedLines.has(line)
  }

  const document = worksheet.document
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
    const isEmpty = document.lineAt(i).isEmptyOrWhitespace && hasDecoration(i)
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

/**
 * Parse and display the result of evaluating part of a worksheet.
 *
 * @see worksheetCreateDecoration
 *
 * @param lineNumber The number of the line in the source that produced the result.
 * @param evalResult The evaluation result.
 * @param worksheet  The worksheet that receives the result.
 * @param editor     The editor where to display the result.
 * @return A `Thenable` that will insert necessary lines to fit the output
 *         and display the decorations upon completion.
 */
function worksheetDisplayResult(lineNumber: number, evalResult: string, worksheet: Worksheet, editor: vscode.TextEditor) {

  const resultLines = evalResult.trim().split(/\r\n|\r|\n/g)
  const margin = worksheet.margin

  // The line where the next decoration should be put.
  // It's the number of the line that produced the output, plus the number
  // of lines that we've inserted so far.
  let actualLine = lineNumber + worksheet.insertedLines

  // If the output has more than one line, we need to insert blank lines
  // below the line that produced the output to fit the output.
  const addNewLinesEdit = new vscode.WorkspaceEdit()
  if (resultLines.length > 1) {
    const linesToInsert = resultLines.length - 1
    const editPos = new vscode.Position(actualLine + 1, 0) // add after the line
    addNewLinesEdit.insert(editor.document.uri, editPos, "\n".repeat(linesToInsert))
    worksheet.insertedLines += linesToInsert
  }

  return vscode.workspace.applyEdit(addNewLinesEdit).then(_ => {
    for (let line of resultLines) {
      const decorationPosition = new vscode.Position(actualLine, 0)
      const decorationMargin = margin - editor.document.lineAt(actualLine).text.length
      const decorationType = worksheetCreateDecoration(decorationMargin, line)
      worksheet.decorationTypes.push(decorationType)
      worksheet.decoratedLines.add(actualLine)

      const decoration = { range: new vscode.Range(decorationPosition, decorationPosition), hoverMessage: line }
      editor.setDecorations(decorationType, [decoration])
      actualLine += 1
    }
  })
}

