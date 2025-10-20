package dotty.tools.pc

import org.eclipse.lsp4j
import org.eclipse.lsp4j.DiagnosticSeverity
import dotty.tools.dotc.interactive.InteractiveDriver
import dotty.tools.dotc.interfaces.Diagnostic as DiagnosticInterfaces
import dotty.tools.dotc.reporting.Diagnostic
import dotty.tools.pc.utils.InteractiveEnrichments.toLsp

import scala.meta.pc.VirtualFileParams
import dotty.tools.dotc.reporting.CodeAction
import dotty.tools.dotc.rewrites.Rewrites.ActionPatch
import scala.jdk.CollectionConverters.*
import dotty.tools.dotc.core.Contexts.Context
import dotty.tools.dotc.reporting.ErrorMessageID
import org.eclipse.lsp4j.DiagnosticTag
import org.eclipse.lsp4j.CodeActionKind

class DiagnosticProvider(driver: InteractiveDriver, params: VirtualFileParams):

  def diagnostics(): List[lsp4j.Diagnostic] =
    if params.shouldReturnDiagnostics then
      val diags = driver.run(params.uri().nn, params.text().nn)
      given Context = driver.currentCtx
      diags.flatMap(toLsp)
    else Nil

  private def toLsp(diag: Diagnostic)(using Context): Option[lsp4j.Diagnostic] =
    Option.when(diag.pos.exists):
      val lspDiag = lsp4j.Diagnostic(
        diag.pos.toLsp,
        diag.msg.message,
        toDiagnosticSeverity(diag.level),
        "presentation compiler",
      )
      lspDiag.setCode(diag.msg.errorId.errorNumber)

      val actions = diag.msg.actions.map(toLspScalaAction).asJava

      // lspDiag.setRelatedInformation(???) Currently not emitted by the compiler
      lspDiag.setData(actions)
      if diag.msg.errorId == ErrorMessageID.UnusedSymbolID then
        lspDiag.setTags(List(DiagnosticTag.Unnecessary).asJava)

      lspDiag

  private def toLspScalaAction(action: CodeAction): lsp4j.CodeAction =
    val lspAction = lsp4j.CodeAction(action.title)
    lspAction.setKind(CodeActionKind.QuickFix)
    lspAction.setIsPreferred(true)
    val edits = action.patches.groupBy(_.srcPos.source.path)
      .map((path, actions) => path -> (actions.map(toLspTextEdit).asJava))
      .asJava

    val workspaceEdit = lsp4j.WorkspaceEdit(edits)
    lspAction.setEdit(workspaceEdit)
    lspAction

  private def toLspTextEdit(actionPatch: ActionPatch): lsp4j.TextEdit =
    val startPos = lsp4j.Position(actionPatch.srcPos.startLine, actionPatch.srcPos.startColumn)
    val endPos   = lsp4j.Position(actionPatch.srcPos.endLine, actionPatch.srcPos.endColumn)
    val range = lsp4j.Range(startPos, endPos)
    lsp4j.TextEdit(range, actionPatch.replacement)

  private def toDiagnosticSeverity(severity: Int): DiagnosticSeverity =
    severity match
      case DiagnosticInterfaces.ERROR   => DiagnosticSeverity.Error
      case DiagnosticInterfaces.WARNING => DiagnosticSeverity.Warning
      case DiagnosticInterfaces.INFO    => DiagnosticSeverity.Information
      case _                            => DiagnosticSeverity.Information
