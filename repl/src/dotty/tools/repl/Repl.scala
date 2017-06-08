package dotty.tools
package repl

import dotc.interactive.{ InteractiveCompiler, InteractiveDriver }
import dotc.reporting.MessageRendering
import dotc.reporting.diagnostic.MessageContainer
import dotc.ast.untpd
import dotc.core.Contexts.Context

class Repl(settings: List[String]) {

  val interactive = new InteractiveDriver(settings)
  val compiler    = new InteractiveCompiler

  def readLine(history: List[String]) =
    (new AmmoniteReader(interactive, compiler, history)).prompt()

  def run(history: List[String] = Nil): Unit =
    readLine(history) match {
      case (Trees(parsedTrees), history) =>
        compile(parsedTrees)
        run(history)

      case (SyntaxErrors(errs, ctx), history) =>
        displaySyntaxErrors(errs)(ctx)
        run(history)

      case (Command(cmd), history) =>
        interpretCommand(cmd, history)

      case (Newline, history) =>
        run(history)
    }

  // Unimplemented:
  def compile(trees: Seq[untpd.Tree]): Unit = ()

  def interpretCommand(cmd: String, history: List[String]): Unit = cmd match {
    case ":quit" => ()
    case _ => run(history)
  }

  private val messageRenderer = new MessageRendering {}
  private def renderMessage(cont: MessageContainer): Context => String =
    messageRenderer.messageAndPos(cont.contained(), cont.pos, messageRenderer.diagnosticLevel(cont))

  def displaySyntaxErrors(errs: Seq[MessageContainer])(implicit ctx: Context): Unit =
    errs.map(renderMessage(_)(ctx)).foreach(println)
}
