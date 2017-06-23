package dotty.tools
package repl

import java.io.PrintStream
import java.net.{URL, URLClassLoader}
import java.lang.ClassLoader

import scala.annotation.tailrec

import dotc.reporting.MessageRendering
import dotc.reporting.diagnostic.MessageContainer
import dotc.ast.untpd
import dotc.ast.tpd
import dotc.core.Contexts.Context
import dotc.core.Flags._
import dotc.core.Denotations.Denotation
import dotc.core.NameKinds.SimpleNameKind
import dotc.core.Types.{ ExprType, ConstantType }
import dotc.config.CompilerCommand
import dotc.{ Compiler, Driver }
import dotc.printing.SyntaxHighlighting
import dotc.repl.AbstractFileClassLoader // FIXME

import AmmoniteReader._
import results._

case class State(objectIndex: Int, valIndex: Int, history: History)

class Repl(
  settings: Array[String],
  parentClassLoader: Option[ClassLoader] = None,
  protected val out: PrintStream = System.out
) extends Driver {

  // FIXME: Change the Driver API to not require implementing this method
  override protected def newCompiler(implicit ctx: Context): Compiler =
    ???

  protected[this] def initializeCtx = {
    val rootCtx = initCtx.fresh
    val summary = CompilerCommand.distill(settings)(rootCtx)
    val ictx = rootCtx.setSettings(summary.sstate)
    ictx.base.initialize()(ictx)
    ictx
  }

  private[this] var _classLoader: ClassLoader = _
  def classLoader(implicit ctx: Context): ClassLoader = {
    if (_classLoader eq null) _classLoader = {
      /** the compiler's classpath, as URL's */
      val compilerClasspath: Seq[URL] = ctx.platform.classPath(ctx).asURLs

      lazy val parent = new URLClassLoader(compilerClasspath.toArray,
                                           classOf[Repl].getClassLoader)

      new AbstractFileClassLoader(compiler.virtualDirectory,
                                  parentClassLoader.getOrElse(parent))
    }

    // Set the current Java "context" class loader to this interpreter's
    // class loader
    Thread.currentThread.setContextClassLoader(_classLoader)
    _classLoader
  }

  private[this] def resetToInitial(): Unit = {
    myCtx = initializeCtx
    compiler = new ReplCompiler(myCtx)
    _classLoader = null
  }

  protected[this] var myCtx: Context         = _
  protected[this] var compiler: ReplCompiler = _

  resetToInitial()

  private def readLine(history: History) =
    AmmoniteReader(history)(myCtx).prompt()

  @tailrec final def run(state: State = State(0, 0, Nil)): Unit =
    readLine(state.history) match {
      case (parsed: Parsed, history) =>
        val newState = compile(parsed, state)
        run(newState.copy(history = history))

      case (SyntaxErrors(errs), history) =>
        displayErrors(errs)(myCtx)
        run(state)

      case (Newline, history) =>
        run(state)

      case (cmd: Command, history) =>
        interpretCommand(cmd, state)
    }

  def compile(parsed: Parsed, state: State): State = {
    implicit val ctx = myCtx
    compiler
      .compile(parsed, state)
      .fold(
        errors => {
          displayErrors(errors)
          state
        },
        (unit, newState, ctx) => {
          displayDefinitions(unit.tpdTree)(ctx)
          newState
        }
      )
  }

  def displayDefinitions(tree: tpd.Tree)(implicit ctx: Context): Unit = {
    def display(tree: tpd.Tree) = if (tree.symbol.info.exists) {
      val info = tree.symbol.info
      val defn = ctx.definitions
      val defs =
        info.bounds.hi.finalResultType
        .membersBasedOnFlags(Method, Accessor | ParamAccessor | Synthetic | Private)
        .filterNot { denot =>
          denot.symbol.owner == defn.AnyClass ||
          denot.symbol.owner == defn.ObjectClass ||
          denot.symbol.isConstructor
        }

      val vals =
        info.fields
        .filterNot(_.symbol.is(ParamAccessor | Private | Synthetic | Module))
        .filter(_.symbol.name.is(SimpleNameKind))

      (
        defs.map(Rendering.renderMethod) ++
        vals.map(Rendering.renderVal(_, classLoader))
      ).foreach(str => out.println(SyntaxHighlighting(str)))
    }

    def displayTypeDef(tree: tpd.TypeDef) = {
      val sym = tree.symbol
      val name = tree.name.show.dropRight(if (sym.is(Module)) 1 else 0)

      val kind =
        if (sym.is(CaseClass)) "case class"
        else if (sym.is(Trait)) "trait"
        else if (sym.is(Module)) "object"
        else "class"

      out.println(
        SyntaxHighlighting(s"// defined ").toString +
        SyntaxHighlighting(s"$kind $name").toString
      )
    }


    tree match {
      case tpd.PackageDef(_, xs) =>
        val allTypeDefs = xs.collect { case td: tpd.TypeDef => td }
        val (objs, tds) = allTypeDefs.partition(_.name.show.startsWith("ReplSession"))
        objs
          .foreach(display)
        tds
          .filterNot(t => t.symbol.is(Synthetic) || !t.name.is(SimpleNameKind))
          .foreach(displayTypeDef)
    }
  }

  def interpretCommand(cmd: Command, state: State): Unit = cmd match {
    case UnknownCommand(cmd) => {
      out.println(s"""Unknown command: "$cmd", run ":help" for a list of commands""")
      run(state)
    }

    case Help => {
      out.println(Help.text)
      run(state)
    }

    case Reset => {
      resetToInitial()
      run()
    }

    case Load(path) =>
      if ((new java.io.File(path)).exists) {
        val contents = scala.io.Source.fromFile(path).mkString
        run(state.copy(history = contents :: state.history))
      }
      else {
        out.println(s"""Couldn't find file "$path"""")
        run(state)
      }

    case Type(expr) => {
      compiler.typeOf(expr, state).fold(
        errors => displayErrors(errors)(myCtx),
        res    => out.println(SyntaxHighlighting(res))
      )
      run(state.copy(history = s":type $expr" :: state.history))
    }

    case Quit =>
      // end of the world!
  }

  /** A `MessageRenderer` without file positions */
  private val messageRenderer = new MessageRendering {
    import dotc.reporting.diagnostic._
    import dotc.util._
    override def messageAndPos(msg: Message, pos: SourcePosition, diagnosticLevel: String)(implicit ctx: Context): String = {
      val sb = scala.collection.mutable.StringBuilder.newBuilder
      val (srcBefore, srcAfter, offset) = sourceLines(pos)
      val marker = columnMarker(pos, offset)
      val err = errorMsg(pos, msg.msg, offset)
      sb.append((srcBefore ::: marker :: err :: outer(pos, " " * (offset - 1)) ::: srcAfter).mkString("\n"))
      sb.toString
    }
  }

  private def renderMessage(cont: MessageContainer): Context => String =
    messageRenderer.messageAndPos(cont.contained(), cont.pos, messageRenderer.diagnosticLevel(cont))

  def displayErrors(errs: Seq[MessageContainer])(implicit ctx: Context): Unit =
    errs.map(renderMessage(_)(ctx)).foreach(out.println)
}
