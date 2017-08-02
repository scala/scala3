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
import dotc.interactive.{ SourceTree, Interactive }
import dotc.core.Contexts.Context
import dotc.core.Flags._
import dotc.core.StdNames._
import dotc.core.Symbols.Symbol
import dotc.core.Denotations.Denotation
import dotc.core.NameKinds.SimpleNameKind
import dotc.core.Types.{ ExprType, ConstantType }
import dotc.config.CompilerCommand
import dotc.{ Compiler, Driver }
import dotc.printing.SyntaxHighlighting
import dotc.repl.AbstractFileClassLoader // FIXME
import dotc.util.Positions.Position

import AmmoniteReader._
import results._

case class State(objectIndex: Int,
                 valIndex: Int,
                 history: History,
                 imports: List[(untpd.Import, String)]) {
  def withHistory(newEntry: String) = copy(history = newEntry :: history)
  def withHistory(h: History) = copy(history = h)
}

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

  private[this] def completions(cursor: Int, expr: String, state: State): (Int, Seq[String], Seq[String]) = {
    import tpd._

    // does the `sym` match the prefix in the `tree`?
    def matchingPrefix(sym: Symbol, tree: Tree)(implicit ctx: Context) =
      tree match {
        case Select(_, id) if id != nme.ERROR =>
          sym.name.startsWith(id.show)
        case _ => true
      }


    // return the correct cursor position and symbols matching prefix
    def findMatching(newCursor: Int, tree: Tree, syms: List[Symbol])(implicit ctx: Context) = {
      val (inCompanion, inInstance) =
        syms
          .filter(matchingPrefix(_, tree))
          .partition(_.owner.is(Module))

      val offset = if (expr.last == '.') 1 else 0
      (newCursor + offset, inInstance.map(_.name.show), inCompanion.map(_.name.show))
    }

    compiler
      .typeCheck(expr, state, errorsAllowed = true)
      .map { (tree, ctx) =>
        val (newCursor, completedTree) = tree.rhs(ctx) match {
          case Block(_, sel @ Select(qual, id)) => (sel.pos.point, sel)
          case _ => (cursor, EmptyTree)
        }
        val file = new dotc.util.SourceFile("compl", expr)
        val srcPos = dotc.util.SourcePosition(file, Position(cursor))
        val completions = Interactive.completions(SourceTree(tree, file) :: Nil, srcPos)(ctx)

        (newCursor, completedTree, completions, ctx)
      }
      .fold(_ => (cursor, Nil, Nil), findMatching(_, _, _)(_))
  }

  private def readLine(state: State) =
    AmmoniteReader(state.history, completions(_, _, state))(myCtx).prompt()

  def extractImports(trees: List[untpd.Tree])(implicit context: Context): List[(untpd.Import, String)] =
    trees.collect { case imp: untpd.Import => (imp, imp.show) }

  @tailrec final def run(state: State = State(0, 0, Nil, Nil)): Unit =
    readLine(state) match {
      case (parsed: Parsed, history) =>
        val newState = compile(parsed, state)
        run(newState.withHistory(history))

      case (SyntaxErrors(errs, _), history) =>
        displayErrors(errs)(myCtx)
        run(state.withHistory(history))

      case (Newline, _) =>
        run(state)

      case (SigKill, _) =>
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
          newState.copy(imports = newState.imports ++ extractImports(parsed.trees)(ctx))
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
      run(state.withHistory(s":$cmd"))
    }

    case Help => {
      out.println(Help.text)
      run(state.withHistory(Help.command))
    }

    case Reset => {
      resetToInitial()
      run()
    }

    case Imports => {
      state.imports foreach { case (_, i) => println(SyntaxHighlighting(i)) }
      run(state.withHistory(Imports.command))
    }

    case Load(path) =>
      val loadCmd = s"${Load.command} $path"
      if ((new java.io.File(path)).exists) {
        val contents = scala.io.Source.fromFile(path).mkString
        ParseResult(contents)(myCtx) match {
          case parsed: Parsed =>
            val newState = compile(parsed, state)
            run(newState.withHistory(loadCmd))
          case SyntaxErrors(errors, _) =>
            displayErrors(errors)(myCtx)
            run(state.withHistory(loadCmd))
          case _ =>
            run(state.withHistory(loadCmd))
        }
      }
      else {
        out.println(s"""Couldn't find file "$path"""")
        run(state.withHistory(loadCmd))
      }

    case Type(expr) => {
      compiler.typeOf(expr, state).fold(
        errors => displayErrors(errors)(myCtx),
        res    => out.println(SyntaxHighlighting(res))
      )
      run(state.withHistory(s"${Type.command} $expr"))
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
