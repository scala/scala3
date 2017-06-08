package dotty.tools
package repl

import dotc.interactive.{ InteractiveCompiler, InteractiveDriver }
import dotc.reporting.diagnostic.MessageContainer
import dotc.ast.untpd
import ParseResult._

class Repl(settings: List[String]) {

  val interactive = new InteractiveDriver(settings)
  val compiler    = new InteractiveCompiler

  def run(history: List[String] = Nil): Unit =
    (new AmmoniteReader(interactive, compiler, history)).prompt() match {
      case (Trees(parsedTrees), history) =>
        compile(parsedTrees)
        run(history)

      case (SyntaxErrors(errs), history) =>
        displayErrors(errs)
        run(history)

      case (Exit, _) => ()
    }

  // Unimplemented:
  def compile(trees: Seq[untpd.Tree]): Unit = ()
  def displayErrors(errs: Seq[MessageContainer]): Unit = ()
}
