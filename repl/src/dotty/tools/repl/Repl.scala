package dotty.tools
package repl

import java.io.{ InputStream, PrintStream }
import java.net.{URL, URLClassLoader}
import java.lang.ClassLoader

import scala.annotation.tailrec

import dotc.reporting.MessageRendering
import dotc.reporting.diagnostic.MessageContainer
import dotc.ast.untpd
import dotc.ast.tpd
import dotc.interactive.{ SourceTree, Interactive }
import dotc.core.Contexts.Context
import dotc.core.Mode
import dotc.core.Flags._
import dotc.core.Types._
import dotc.core.StdNames._
import dotc.core.Names.Name
import dotc.core.NameOps._
import dotc.core.Symbols.{ Symbol, NoSymbol, defn }
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

/** The state of the REPL */
case class State(objectIndex: Int,
                 valIndex: Int,
                 history: History,
                 imports: List[(untpd.Import, String)]) {
  def withHistory(newEntry: String) = copy(history = newEntry :: history)
  def withHistory(h: History) = copy(history = h)
}

object State { val init = State(0, 0, Nil, Nil) }

/** A list of possible completions at index `cursor` */
case class Completions(cursor: Int,
                       instance: List[String],
                       companion: List[String])

/** Main REPL instance, orchestrating input, compilation and presentation */
class Repl(
  settings: Array[String],
  parentClassLoader: Option[ClassLoader] = None,
  protected val out: PrintStream = System.out
) extends Driver {

  // FIXME: Change the Driver API to not require implementing this method
  override protected def newCompiler(implicit ctx: Context): Compiler =
    ???

  /** Create a fresh and initialized context with IDE mode enabled */
  protected[this] def initializeCtx = {
    val rootCtx = initCtx.fresh
    val summary = CompilerCommand.distill(settings)(rootCtx)
    val ictx = rootCtx.setSettings(summary.sstate)
      .addMode(Mode.ReadPositions).addMode(Mode.Interactive)
    ictx.base.initialize()(ictx)
    ictx
  }

  private[this] var _classLoader: ClassLoader = _
  protected[this] final def classLoader(implicit ctx: Context): ClassLoader = {
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

  /** Reset state of repl to the initial state
   *
   *  This method is responsible for performing an all encompassing reset. As
   *  such, when the user enters `:reset` this method should be called to reset
   *  everything properly
   */
  protected[this] def resetToInitial(): Unit = {
    myCtx = initializeCtx
    compiler = new ReplCompiler(myCtx)
    _classLoader = null
  }

  protected[this] var myCtx: Context         = _
  protected[this] var compiler: ReplCompiler = _

  // initialize the REPL session as part of the constructor so that once `run`
  // is called, we're in business
  resetToInitial()

  /** Run REPL with `state` until `:quit` command found
   *
   *  This method is the main entry point into the REPL. Its effects are not
   *  observable outside of the CLI, for this reason, most helper methods are
   *  `protected final` to facilitate testing.
   */
  @tailrec final def runUntilQuit(state: State = State.init): State = {
    val res = readLine(state)
    if (res.isInstanceOf[Quit.type]) state
    else runUntilQuit(interpret(res, state))
  }

  final def run(input: String, state: State): State =
    run(ParseResult(input)(myCtx), state)

  final def run(res: ParseResult, state: State): State =
    interpret(res, state)

  /** Extract possible completions at the index of `cursor` in `expr` */
  protected[this] final def completions(cursor: Int, expr: String, state: State): Completions = {
    import tpd._

    // does the `sym` match the prefix in the `tree`?
    def matchingPrefix(sym: Symbol, tree: Tree)(implicit ctx: Context) =
      tree match {
        case Select(_, id) if id != nme.ERROR =>
          sym.name.startsWith(id.show)
        case _ => true
      }

    // completing a Module e.g: "List.ra<tab>"?
    def completingModule(tree: Tree)(implicit ctx: Context) = tree match {
      case Select(x, _) =>
        x.tpe match {
          case tpe: NamedType => tpe.isTerm
          case _ => false
        }
      case _ => false
    }

    // return the correct cursor position and symbols matching prefix
    def findMatching(newCursor: Int, tree: Tree, syms: List[Symbol])(implicit ctx: Context) = {
      val (inCompanion, inInstance) =
        syms
          .filter(matchingPrefix(_, tree))
          .partition(_ => completingModule(tree))

      val offset = if (expr.last == '.') 1 else 0

      Completions(
        newCursor + offset,
        inInstance.map(_.name.show).distinct,
        inCompanion.map(_.name.show).distinct
      )
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
      .fold(_ => Completions(cursor, Nil, Nil), findMatching(_, _, _)(_))
  }

  /** Blockingly read a line, getting back a parse result and new history */
  private def readLine(state: State): ParseResult =
    AmmoniteReader(out, state.history, completions(_, _, state))(myCtx).prompt

  private def extractImports(trees: List[untpd.Tree])(implicit context: Context): List[(untpd.Import, String)] =
    trees.collect { case imp: untpd.Import => (imp, imp.show) }

  private def interpret(res: ParseResult, state: State): State =
    res match {
      case parsed: Parsed =>
        compile(parsed, state).withHistory(parsed.sourceCode :: state.history)

      case SyntaxErrors(src, errs, _) =>
        displayErrors(errs)(myCtx)
        state.withHistory(src :: state.history)

      case Newline | SigKill => state

      case cmd: Command =>
        interpretCommand(cmd, state)
    }

  /** Compile `parsed` trees and evolve `state` in accordance */
  protected[this] final def compile(parsed: Parsed, state: State): State = {
    def extractNewestWrapper(tree: untpd.Tree): Name = tree match {
      case untpd.PackageDef(_, (obj: untpd.ModuleDef) :: Nil) => obj.name.moduleClassName
      case _ => nme.NO_NAME
    }

    implicit val ctx = myCtx
    compiler
      .compile(parsed, state)
      .fold(
        errors => {
          displayErrors(errors)
          state
        },
        (unit, newState, ctx) => {
          val newestWrapper =
            extractNewestWrapper(unit.untpdTree)
          val newStateWithImports =
            newState.copy(imports = newState.imports ++ extractImports(parsed.trees)(ctx))

          displayDefinitions(unit.tpdTree, newestWrapper, newStateWithImports)(ctx)
          newStateWithImports
        }
      )
  }

  /** Display definitions from `tree` */
  private def displayDefinitions(tree: tpd.Tree, newestWrapper: Name, state: State)(implicit ctx: Context): Unit = {
    def displayMembers(symbol: Symbol) = if (tree.symbol.info.exists) {
      val info = symbol.info
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

      val typeAliases =
        info.bounds.hi.typeMembers.filter(_.symbol.info.isInstanceOf[TypeAlias])

      (
        typeAliases.map("// defined alias " + _.symbol.showUser) ++
        defs.map(Rendering.renderMethod) ++
        vals.map(Rendering.renderVal(_, classLoader))
      ).foreach(str => out.println(SyntaxHighlighting(str)))
    }

    def isSyntheticCompanion(sym: Symbol) = sym.is(Module) && sym.is(Synthetic)

    def displayTypeDefs(sym: Symbol) = sym.info.memberClasses
      .filterNot(x => isSyntheticCompanion(x.symbol))
      .foreach { td =>
        out.println(SyntaxHighlighting("// defined " + td.symbol.showUser))
      }

    ctx.atPhase(ctx.typerPhase.next) { implicit ctx =>
      tree.symbol.info.memberClasses
        .find(_.symbol.name == newestWrapper.moduleClassName)
        .map { wrapperModule =>
          displayTypeDefs(wrapperModule.symbol)
          displayMembers(wrapperModule.symbol)
        }
        .getOrElse(println(
          s"""couldn't find wrapper symbol: $newestWrapper, please report this message:
             |
             |tree.symbol.info.memberClasses: ${tree.symbol.info.memberClasses}
             |
             |${state.history.mkString("\n")}
           """.stripMargin
        ))
    }
  }

  /** Interpret `cmd` to action and propagate potentially new `state` */
  private def interpretCommand(cmd: Command, state: State): State = cmd match {
    case UnknownCommand(cmd) => {
      out.println(s"""Unknown command: "$cmd", run ":help" for a list of commands""")
      state.withHistory(s":$cmd")
    }

    case Help => {
      out.println(Help.text)
      state.withHistory(Help.command)
    }

    case Reset => {
      resetToInitial()
      State.init
    }

    case Imports => {
      state.imports foreach { case (_, i) => println(SyntaxHighlighting(i)) }
      state.withHistory(Imports.command)
    }

    case Load(path) =>
      val loadCmd = s"${Load.command} $path"
      if ((new java.io.File(path)).exists) {
        val contents = scala.io.Source.fromFile(path).mkString
        ParseResult(contents)(myCtx) match {
          case parsed: Parsed =>
            compile(parsed, state).withHistory(loadCmd)
          case SyntaxErrors(_, errors, _) =>
            displayErrors(errors)(myCtx)
            state.withHistory(loadCmd)
          case _ =>
            state.withHistory(loadCmd)
        }
      }
      else {
        out.println(s"""Couldn't find file "$path"""")
        state.withHistory(loadCmd)
      }

    case Type(expr) => {
      compiler.typeOf(expr, state).fold(
        errors => displayErrors(errors)(myCtx),
        res    => out.println(SyntaxHighlighting(res))
      )
      state.withHistory(s"${Type.command} $expr")
    }

    case Quit =>
      // end of the world!
      state
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

  /** Render messages using the `MessageRendering` trait */
  private def renderMessage(cont: MessageContainer): Context => String =
    messageRenderer.messageAndPos(cont.contained(), cont.pos, messageRenderer.diagnosticLevel(cont))

  /** Output errors to `out` */
  private def displayErrors(errs: Seq[MessageContainer])(implicit ctx: Context): Unit =
    errs.map(renderMessage(_)(ctx)).foreach(out.println)
}
