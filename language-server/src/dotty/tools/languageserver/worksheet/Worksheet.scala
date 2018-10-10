package dotty.tools.languageserver.worksheet

import dotty.tools.dotc.ast.tpd.{DefTree, Template, Tree, TypeDef}
import dotty.tools.dotc.core.Contexts.Context
import dotty.tools.dotc.interactive.SourceTree
import dotty.tools.dotc.util.Positions.Position
import dotty.tools.dotc.util.SourceFile

import dotty.tools.dotc.core.Flags.Synthetic

import org.eclipse.lsp4j.jsonrpc.CancelChecker

import java.util.concurrent.CancellationException

object Worksheet {

  /**
   * Run `tree` as a worksheet using the REPL.
   *
   * @param tree          The top level object wrapping the worksheet.
   * @param sendMessage   A mean of communicating the results of evaluation back.
   * @param cancelChecker A token to check whether execution should be cancelled.
   */
  def run(tree: SourceTree,
          sendMessage: (Int, String) => Unit,
          cancelChecker: CancelChecker)(
      implicit ctx: Context): Unit = synchronized {

    Evaluator.get(cancelChecker) match {
      case None =>
        sendMessage(1, "Couldn't start JVM.")
      case Some(evaluator) =>
        tree.tree match {
          case td @ TypeDef(_, template: Template) =>
            val executed = collection.mutable.Set.empty[(Int, Int)]

            template.body.foreach {
              case statement: DefTree if statement.symbol.is(Synthetic) =>
                ()

              case statement if evaluator.isAlive() && executed.add(bounds(statement.pos)) =>
                try {
                  cancelChecker.checkCanceled()
                  val (line, result) = execute(evaluator, statement, tree.source)
                  if (result.nonEmpty) sendMessage(line, result)
                } catch { case _: CancellationException => () }

              case _ =>
                ()
            }
        }
    }
  }

  /**
   * Extract `tree` from the source and evaluate it in the REPL.
   *
   * @param evaluator  The JVM that runs the REPL.
   * @param tree       The compiled tree to evaluate.
   * @param sourcefile The sourcefile of the worksheet.
   * @return The line in the sourcefile that corresponds to `tree`, and the result.
   */
  private def execute(evaluator: Evaluator, tree: Tree, sourcefile: SourceFile): (Int, String) = {
    val source = sourcefile.content.slice(tree.pos.start, tree.pos.end).mkString
    val line = sourcefile.offsetToLine(tree.pos.end)
    (line, evaluator.eval(source).getOrElse(""))
  }

  private def bounds(pos: Position): (Int, Int) = (pos.start, pos.end)

}

