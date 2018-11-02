package dotty.tools.languageserver.worksheet

import dotty.tools.dotc.ast.tpd.{DefTree, Template, Tree, TypeDef}
import dotty.tools.dotc.core.Contexts.ContextRenamed
import dotty.tools.dotc.interactive.SourceTree
import dotty.tools.dotc.util.Positions.Position
import dotty.tools.dotc.util.SourceFile

import dotty.tools.dotc.core.Flags.Synthetic

import org.eclipse.lsp4j.jsonrpc.CancelChecker

object Worksheet {

  /**
   * Run `tree` as a worksheet using the REPL.
   *
   * @param tree          The top level object wrapping the worksheet.
   * @param treeLock      Object on which to lock when doing operations on trees.
   * @param sendMessage   A mean of communicating the results of evaluation back.
   * @param cancelChecker A token to check whether execution should be cancelled.
   */
  def run(tree: SourceTree,
          treeLock: Object,
          sendMessage: (Int, String) => Unit,
          cancelChecker: CancelChecker)(
    implicit ctx: ContextRenamed): Unit = {
    // For now, don't try to run multiple evaluators in parallel, this would require
    // changes to the logic of Evaluator.get among other things.
    Evaluator.synchronized {
      Evaluator.get(cancelChecker) match {
        case None =>
          sendMessage(1, "Couldn't start the JVM.")
        case Some(evaluator) =>
          val queries = treeLock.synchronized {
            tree.tree match {
              case td @ TypeDef(_, template: Template) =>
                val seen = collection.mutable.Set.empty[(Int, Int)]

                template.body.flatMap {
                  case statement: DefTree if statement.symbol.is(Synthetic) =>
                    None
                  case statement if seen.add(bounds(statement.pos)) =>
                    Some(query(statement, tree.source))
                  case _ =>
                    None
                }
            }
          }
          queries.foreach { (line, code) =>
            cancelChecker.checkCanceled()
            val res = evaluator.eval(code)
            cancelChecker.checkCanceled()
            if (res.nonEmpty)
              sendMessage(line, res)
          }
      }
    }
  }

  /**
   * Extract the line number and source code corresponding to this tree
   *
   * @param evaluator  The JVM that runs the REPL.
   * @param tree       The compiled tree to evaluate.
   * @param sourcefile The sourcefile of the worksheet.
   */
  private def query(tree: Tree, sourcefile: SourceFile): (Int, String) = {
    val line = sourcefile.offsetToLine(tree.pos.end)
    val source = sourcefile.content.slice(tree.pos.start, tree.pos.end).mkString
    (line, source)
  }

  private def bounds(pos: Position): (Int, Int) = (pos.start, pos.end)

}

