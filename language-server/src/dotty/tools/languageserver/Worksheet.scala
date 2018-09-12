package dotty.tools.languageserver

import dotty.tools.dotc.ast.tpd.{Template, Tree, TypeDef}
import dotty.tools.dotc.core.Contexts.Context
import dotty.tools.dotc.interactive.SourceTree
import dotty.tools.dotc.util.SourceFile

import dotty.tools.repl.{ReplDriver, State}

import java.io.{ByteArrayOutputStream, PrintStream}

object Worksheet {

  /**
   * Evaluate `tree` as a worksheet using the REPL.
   *
   * @param tree        The top level object wrapping the worksheet.
   * @param sendMessage A mean of communicating the results of evaluation back.
   */
  def evaluate(tree: SourceTree, sendMessage: String => Unit)(implicit ctx: Context): Unit = {
    tree.tree match {
      case td @ TypeDef(_, template: Template) =>
        val replOut = new ByteArrayOutputStream
        val repl = new ReplDriver(replOptions, out = new PrintStream(replOut))

        template.body.foldLeft(repl.initialState) {
          case (state, statement) =>
            val (line, newState) = execute(repl, state, statement, tree.source)
            val result = new String(replOut.toByteArray())
            sendMessage(encode(result, line))
            replOut.reset()
            newState
        }
    }
  }

  /**
   * Extract `tree` from the source and evaluate it in the REPL.
   *
   * @param repl       The REPL that will evaluate the worksheet.
   * @param state      Current state of the REPL.
   * @param tree       The compiled tree to evaluate.
   * @param sourcefile The sourcefile of the worksheet.
   * @return The line in the sourcefile that corresponds to `tree` and the new state of the REPL.
   */
  private def execute(repl: ReplDriver, state: State, tree: Tree, sourcefile: SourceFile)(implicit ctx: Context): (Int, State) = {
    val source = sourcefile.content.slice(tree.pos.start, tree.pos.end).mkString
    val line = sourcefile.offsetToLine(tree.pos.end)
    (line, repl.run(source)(state))
  }

  private def replOptions(implicit ctx: Context): Array[String] =
    Array("-color:never", "-classpath", ctx.settings.classpath.value)

  private def encode(message: String, line: Int): String =
    line + ":" + message
}
