package dotty.tools
package repl

import scala.annotation.tailrec

import dotc.reporting.MessageRendering
import dotc.reporting.diagnostic.MessageContainer
import dotc.ast.untpd
import dotc.core.Contexts.Context
import dotc.config.CompilerCommand
import dotc.{ Compiler, Driver }

import AmmoniteReader._
import results._

class Repl(settings: Array[String]) extends Driver {

  // FIXME: Change the Driver API to not require implementing this method
  override protected def newCompiler(implicit ctx: Context): Compiler =
    ???

  private[this] def initializeCtx = {
    val rootCtx = initCtx.fresh
    val summary = CompilerCommand.distill(settings)(rootCtx)
    val ictx = rootCtx.setSettings(summary.sstate)
    ictx.base.initialize()(ictx)
    ictx
  }

  private[this] var myCtx    = initializeCtx
  private[this] var typer    = new ReplTyper(myCtx)
  private[this] var compiler = new ReplCompiler(myCtx)

  private def readLine(history: History) =
    AmmoniteReader(history)(myCtx).prompt()

  @tailrec
  final def run(history: History = Nil,
                tree: InjectableTree = InjectableTree()(myCtx)): Unit =
    readLine(history) match {
      case (parsed: Parsed, history) =>
        val newTree = compile(parsed, tree)(myCtx)
        run(history, newTree)

      case (SyntaxErrors(errs, ctx), history) =>
        displayErrors(errs)(ctx)
        run(history)

      case (Newline, history) =>
        run(history)

      case (cmd: Command, history) =>
        interpretCommand(cmd, history)
    }

  def compile(parsed: Parsed, tree: InjectableTree)(implicit ctx: Context): InjectableTree = {
    val res = for {
      typed    <- typer.typeCheck(parsed, tree.nextId)
      injected <- InjectableTree.patch(tree, typed)
      _        <- compiler.compile(injected.obj)
    } yield injected

    // Fold over result, on failure - report errors and return old `tree`:
    res.fold(
      errors => {
        displayErrors(errors.msgs)
        tree
      },
      id
    )
  }

  def interpretCommand(cmd: Command, history: History): Unit = cmd match {
    case UnknownCommand(cmd) => {
      println(s"""Unknown command: "$cmd", run ":help" for a list of commands""")
      run(history)
    }

    case Help => {
      println(Help.text)
      run(history)
    }

    case Reset => {
      myCtx = initCtx.fresh
      run(Nil)
    }

    case Load(path) =>
      if ((new java.io.File(path)).exists) {
        val contents = scala.io.Source.fromFile(path).mkString
        run(contents :: history)
      }
      else {
        println(s"""Couldn't find file "$path"""")
        run(history)
      }

    case Quit =>
      // end of the world!
  }

  private val messageRenderer = new MessageRendering {}
  private def renderMessage(cont: MessageContainer): Context => String =
    messageRenderer.messageAndPos(cont.contained(), cont.pos, messageRenderer.diagnosticLevel(cont))

  def displayErrors(errs: Seq[MessageContainer])(implicit ctx: Context): Unit =
    errs.map(renderMessage(_)(ctx)).foreach(println)
}
