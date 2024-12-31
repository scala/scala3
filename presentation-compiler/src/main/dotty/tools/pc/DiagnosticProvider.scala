package dotty.tools.pc

import org.eclipse.lsp4j
import org.eclipse.lsp4j.DiagnosticSeverity
import dotty.tools.dotc.interactive.InteractiveDriver
import dotty.tools.dotc.interfaces.Diagnostic as DiagnosticInterfaces
import dotty.tools.dotc.reporting.Diagnostic
import dotty.tools.pc.utils.InteractiveEnrichments.toLsp

import scala.meta.pc.VirtualFileParams
import ch.epfl.scala.bsp4j
import dotty.tools.dotc.reporting.CodeAction
import dotty.tools.dotc.rewrites.Rewrites.ActionPatch
import scala.jdk.CollectionConverters.*
import dotty.tools.dotc.core.Contexts.Context
import dotty.tools.dotc.reporting.ErrorMessageID
import org.eclipse.lsp4j.DiagnosticTag

class DiagnosticProvider(driver: InteractiveDriver, params: VirtualFileParams):

  def diagnostics(): List[lsp4j.Diagnostic] =
    val diags = driver.run(params.uri().nn, params.text().nn)
    given Context = driver.currentCtx
    diags.flatMap(toLsp)

  private def toLsp(diag: Diagnostic)(using Context): Option[lsp4j.Diagnostic] =
    Option.when(diag.pos.exists):
      val lspDiag = lsp4j.Diagnostic(
        diag.pos.toLsp,
        diag.msg.message,
        toDiagnosticSeverity(diag.level),
        "presentation compiler",
      )
      lspDiag.setCode(diag.msg.errorId.errorNumber)

      val scalaDiagnostic = new bsp4j.ScalaDiagnostic()
      val actions = diag.msg.actions.map(toBspScalaAction).asJava
      scalaDiagnostic.setActions(actions)
      // lspDiag.setRelatedInformation(???) Currently not emitted by the compiler
      lspDiag.setData(scalaDiagnostic)
      if diag.msg.errorId == ErrorMessageID.UnusedSymbolID then
        lspDiag.setTags(List(DiagnosticTag.Unnecessary).asJava)

      lspDiag

  private def toBspScalaAction(action: CodeAction): bsp4j.ScalaAction =
    val bspAction = bsp4j.ScalaAction(action.title)
    action.description.foreach(bspAction.setDescription)
    val workspaceEdit = bsp4j.ScalaWorkspaceEdit(action.patches.map(toBspTextEdit).asJava)
    bspAction.setEdit(workspaceEdit)
    bspAction

  private def toBspTextEdit(actionPatch: ActionPatch): bsp4j.ScalaTextEdit =
    val startPos = bsp4j.Position(actionPatch.srcPos.startLine, actionPatch.srcPos.startColumn)
    val endPos   = bsp4j.Position(actionPatch.srcPos.endLine, actionPatch.srcPos.endColumn)
    val range = bsp4j.Range(startPos, endPos)
    bsp4j.ScalaTextEdit(range, actionPatch.replacement)


  private def toDiagnosticSeverity(severity: Int): DiagnosticSeverity =
    severity match
      case DiagnosticInterfaces.ERROR   => DiagnosticSeverity.Error
      case DiagnosticInterfaces.WARNING => DiagnosticSeverity.Warning
      case DiagnosticInterfaces.INFO    => DiagnosticSeverity.Information
      case _                            => DiagnosticSeverity.Information

