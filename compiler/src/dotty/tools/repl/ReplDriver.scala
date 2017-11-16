package dotty.tools
package repl

import java.io.{InputStream, PrintStream}

import scala.annotation.tailrec
import dotc.reporting.MessageRendering
import dotc.reporting.diagnostic.MessageContainer
import dotc.ast.untpd
import dotc.ast.tpd
import dotc.interactive.{Interactive, SourceTree}
import dotc.core.Contexts.Context
import dotc.{CompilationUnit, Run}
import dotc.core.{Denotations, Mode}
import dotc.core.Flags._
import dotc.core.Types._
import dotc.core.StdNames._
import dotc.core.Names.Name
import dotc.core.NameOps._
import dotc.core.Symbols.{NoSymbol, Symbol, defn}
import dotc.core.Denotations.Denotation
import dotc.core.Types.{ConstantType, ExprType}
import dotc.core.NameKinds.SimpleNameKind
import dotc.config.CompilerCommand
import dotc.{Compiler, Driver}
import dotc.printing.SyntaxHighlighting
import dotc.util.Positions.Position
import io._
import AmmoniteReader._
import results._

/** The state of the REPL contains necessary bindings instead of having to have
 *  mutation
 *
 *  The compiler in the REPL needs to do some wrapping in order to compile
 *  valid code. This wrapping occurs when a single `MemberDef` that cannot be
 *  top-level needs to be compiled. In order to do this, we need some unique
 *  identifier for each of these wrappers. That identifier is `objectIndex`.
 *
 *  Free expressions such as `1 + 1` needs to have an assignment in order to be
 *  of use. These expressions are therefore given a identifier on the format
 *  `resX` where `X` starts at 0 and each new expression that needs an
 *  identifier is given the increment of the old identifier. This identifier is
 *  `valIndex`.
 *
 *  @param objectIndex the index of the next wrapper
 *  @param valIndex the index of next value binding for free expressions
 *  @param history a list of user inputs as strings
 *  @param imports a list of tuples of imports on tree form and shown form
 *  @param run the latest run initiated at the start of interpretation. This
 *             run and its context should be used in order to perform any
 *             manipulation on `Tree`s and `Symbol`s.
 */
case class State(objectIndex: Int,
                 valIndex: Int,
                 history: History,
                 imports: List[(untpd.Import, String)],
                 run: Run) {

  def withHistory(newEntry: String) = copy(history = newEntry :: history)

  def withHistory(h: History) = copy(history = h)

  def newRun(comp: ReplCompiler, rootCtx: Context): State =
    copy(run = comp.newRun(rootCtx, objectIndex))
}

/** A list of possible completions at the index of `cursor`
 *
 *  @param cursor the index of the users cursor in the input
 *  @param suggestions the suggested completions as a filtered list of strings
 */
case class Completions(cursor: Int,
                       suggestions: List[String],
                       details: List[String])

/** Main REPL instance, orchestrating input, compilation and presentation */
class ReplDriver(settings: Array[String],
                 protected val out: PrintStream = System.out,
                 protected val classLoader: Option[ClassLoader] = None,
                 initialCommands: Option[String] = None,
                 cleanupCommands: Option[String] = None) extends Driver {

  /** Overridden to `false` in order to not have to give sources on the
   *  commandline
   */
  override def sourcesRequired = false

  /** Create a fresh and initialized context with IDE mode enabled */
  private[this] def initialCtx = {
    val rootCtx = initCtx.fresh.addMode(Mode.ReadPositions).addMode(Mode.Interactive)
    val ictx = setup(settings, rootCtx)._2.fresh
    ictx.base.initialize()(ictx)
    ictx
  }

  /** the initial, empty state of the REPL session */
  protected[this] def initState = State(0, 0, Nil, Nil, compiler.newRun(rootCtx, 0))

  /** Reset state of repl to the initial state
   *
   *  This method is responsible for performing an all encompassing reset. As
   *  such, when the user enters `:reset` this method should be called to reset
   *  everything properly
   */
  protected[this] def resetToInitial(): Unit = {
    rootCtx = initialCtx
    val outDir: AbstractFile = {
      if (rootCtx.settings.outputDir.isDefault(rootCtx))
        new VirtualDirectory("(memory)", None)
      else {
        val path = Directory(rootCtx.settings.outputDir.value(rootCtx))
        assert(path.isDirectory)
        new PlainDirectory(path)
      }
    }
    compiler = new ReplCompiler(outDir)
    rendering = new Rendering(compiler, classLoader)
  }

  protected[this] var rootCtx: Context = _
  protected[this] var compiler: ReplCompiler = _
  protected[this] var rendering: Rendering = _

  // initialize the REPL session as part of the constructor so that once `run`
  // is called, we're in business
  resetToInitial()

  /** Run REPL with `state` until `:quit` command found
    *
    *  This method is the main entry point into the REPL. Its effects are not
    *  observable outside of the CLI, for this reason, most helper methods are
    *  `protected final` to facilitate testing.
    */
  final def runUntilQuit(initialState: State = initState): State = {
    @tailrec def run(state: State = initState): State = {
      val res = readLine()(state)

      if (res == Quit) {
        out.println()
        state
      }
      else {
        // readLine potentially destroys the run, so a new one is needed for the
        // rest of the interpretation:
        implicit val freshState = state.newRun(compiler, rootCtx)
        run(interpret(res))
      }
    }

    val state = runBootstrapCommands(initialCommands)(initialState)
    val userState = run(state)
    runBootstrapCommands(cleanupCommands)(userState)
  }

  final def run(input: String)(implicit state: State): State =
    run(ParseResult(input)(state.run.runContext))(state.newRun(compiler, rootCtx))

  final def run(res: ParseResult)(implicit state: State): State =
    interpret(res)

  final def runBootstrapCommands(cmds: Option[String])(implicit state: State): State = {
    cmds.map(ParseResult.apply(_)(rootCtx)).map(Silent.apply(_)).foldLeft(state) { (s, cmd) =>
      interpret(cmd)(s)
    }
  }

  /** Extract possible completions at the index of `cursor` in `expr` */
  protected[this] final def completions(cursor: Int, expr: String, state0: State): Completions = {
    // TODO move some of this logic to `Interactive`
    implicit val state = state0.newRun(compiler, rootCtx)
    compiler
      .typeCheck(expr, errorsAllowed = true)
      .map { tree =>
        implicit val ctx: Context = state.run.runContext
        val file = new dotc.util.SourceFile("compl", expr)
        val srcPos = dotc.util.SourcePosition(file, Position(cursor))
        val (startOffset, completions) = Interactive.completions(SourceTree(tree, file) :: Nil, srcPos)(ctx)
        val query =
          if (startOffset < cursor) expr.substring(startOffset, cursor) else ""

        def filterCompletions(name: String) =
          (query == "." || name.startsWith(query)) && name != query


        Completions(
          Math.min(startOffset, cursor) + { if (query == ".") 1 else 0 },
          completions.map(_.name.show).distinct.filter(filterCompletions),
          Nil
        )
      }
      .fold(_ => Completions(cursor, Nil, Nil), x => x)
  }

  /** Blockingly read a line, getting back a parse result and new history */
  private def readLine()(implicit state: State): ParseResult =
    AmmoniteReader(out, state.history, completions(_, _, state))(state.run.runContext).prompt

  private def extractImports(trees: List[untpd.Tree])(implicit context: Context): List[(untpd.Import, String)] =
    trees.collect { case imp: untpd.Import => (imp, imp.show) }

  private def interpret(res: Parsing)(implicit state: State): State = {
    val (parseResult, isSilent) = res match {
      case Silent(x) => (x, true)
      case x: ParseResult => (x, false)
    }

    parseResult match {
      case parsed: Parsed if parsed.trees.nonEmpty =>
        compile(parsed, isSilent)
          .withHistory(parsed.sourceCode :: state.history)
          .newRun(compiler, rootCtx)

      case SyntaxErrors(src, errs, _) =>
        displayErrors(errs)
        state.withHistory(src :: state.history)

      case cmd: Command => interpretCommand(cmd, isSilent)

      case SigKill => // TODO
        state

      case _ => // new line, empty tree
        state
    }
  }

  /** Compile `parsed` trees and evolve `state` in accordance */
  protected[this] final def compile(parsed: Parsed, silent: Boolean = false)(implicit state: State): State = {
    import dotc.ast.Trees.PackageDef
    import untpd.{ PackageDef => _, _ }
    def extractNewestWrapper(tree: Tree): Name = tree match {
      case PackageDef(_, (obj: ModuleDef) :: Nil) => obj.name.moduleClassName
      case _ => nme.NO_NAME
    }

    compiler
      .compile(parsed)
      .fold(
        {
          case (errors: Errors) =>
            displayErrors(errors)
        },
        {
          case (unit: CompilationUnit, newState: State) => {
            val newestWrapper = extractNewestWrapper(unit.untpdTree)
            val newImports = newState.imports ++ extractImports(parsed.trees)(newState.run.runContext)
            val newStateWithImports = newState.copy(imports = newImports)

            val (decls, optIndex, symbol) = getDefinitions(unit.tpdTree, newestWrapper)(newStateWithImports)
            if (!silent) displayDefinitions(decls, symbol)(newStateWithImports)

            optIndex.map(i => newStateWithImports.copy(valIndex = i)).getOrElse(newStateWithImports)
          }
        }
      )
  }

  /** Display definitions from `symbol` */
  private def displayDefinitions(decls: Seq[String], symbol: Option[Symbol])(implicit state: State): Unit = {
    implicit val ctx = state.run.runContext

    def displayMembers(decls: Seq[String]): Unit = {
      decls.foreach(str => out.println(SyntaxHighlighting(str)))
    }

    def isSyntheticCompanion(sym: Symbol) =
      sym.is(Module) && sym.is(Synthetic)

    def displayTypeDefs(optSymbol: Option[Symbol]): Unit = optSymbol.foreach(sym => sym.info.memberClasses
      .collect {
        case x if !isSyntheticCompanion(x.symbol) && !x.symbol.name.isReplWrapperName =>
          x.symbol
      }
      .foreach { sym =>
        out.println(SyntaxHighlighting("// defined " + sym.showUser))
      })
    displayTypeDefs(symbol)
    displayMembers(decls)
  }

  /** get definitions from `tree` */
  private def getDefinitions(tree: tpd.Tree, newestWrapper: Name)(implicit state: State): (Seq[String], Option[Int], Option[Symbol]) = {
    implicit val ctx = state.run.runContext

    def resAndUnit(denot: Denotation) = {
      import scala.util.{ Try, Success }
      val sym = denot.symbol
      val name = sym.name.show
      val hasValidNumber = Try(name.drop(3).toInt) match {
        case Success(num) => num < state.valIndex
        case _ => false
      }
      name.startsWith(str.REPL_RES_PREFIX) && hasValidNumber && sym.info == defn.UnitType
    }

    def getDeclarationsAndIndex(info: Type): (Seq[String], Int) = {
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

      val declarations = typeAliases.map("// defined alias " + _.symbol.showUser) ++
        defs.map(rendering.renderMethod) ++
        vals.map(rendering.renderVal).flatten

      (declarations, state.valIndex - vals.count(resAndUnit))
    }

    ctx.atPhase(ctx.typerPhase.next) { implicit ctx =>

      // get members of wrapped module:
      tree.symbol.info.memberClasses
        .find(_.symbol.name == newestWrapper.moduleClassName)
        .map { wrapperModule =>
          if (tree.symbol.info.exists) {
            val (decls, index) = getDeclarationsAndIndex(wrapperModule.symbol.info)
            (decls, Some(index), Some(wrapperModule.symbol))
          } else (Seq.empty, None, None)
        }
        .getOrElse {
          // user defined a trait/class/object, so no module needed
          (Seq.empty, None, None)
        }
    }
  }

  /** Interpret `cmd` to action and propagate potentially new `state` */
  private def interpretCommand(cmd: Command, silent: Boolean)(implicit state: State): State = cmd match {
    case UnknownCommand(cmd) => {
      if (!silent) out.println(s"""Unknown command: "$cmd", run ":help" for a list of commands""")
      state.withHistory(s"$cmd")
    }

    case Help => {
      if (!silent) out.println(Help.text)
      state.withHistory(Help.command)
    }

    case Reset => {
      resetToInitial()
      initState
    }

    case Imports => {
      if (!silent) state.imports foreach { case (_, i) => println(SyntaxHighlighting(i)) }
      state.withHistory(Imports.command)
    }

    case Load(path) =>
      val loadCmd = s"${Load.command} $path"
      val file = new java.io.File(path)
      if (file.exists) {
        val contents = scala.io.Source.fromFile(path).mkString
        ParseResult(contents)(state.run.runContext) match {
          case parsed: Parsed => compile(parsed, silent)
          case SyntaxErrors(_, errors, _) => displayErrors(errors)
          case _ => state
        }
      }.withHistory(loadCmd)
      else {
        out.println(s"""Couldn't find file "${file.getCanonicalPath}"""")
        state
      }.withHistory(loadCmd)

    case TypeOf(expr) => {
      compiler.typeOf(expr).fold(
        displayErrors,
        res => if (!silent) out.println(SyntaxHighlighting(res))
      )
      state.withHistory(s"${TypeOf.command} $expr")
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
      if (pos.exists) {
        val (srcBefore, srcAfter, offset) = sourceLines(pos)
        val marker = columnMarker(pos, offset)
        val err = errorMsg(pos, msg.msg, offset)
        sb.append((srcBefore ::: marker :: err :: outer(pos, " " * (offset - 1)) ::: srcAfter).mkString("\n"))
      }
      sb.toString
    }
  }

  /** Render messages using the `MessageRendering` trait */
  private def renderMessage(cont: MessageContainer): Context => String =
    messageRenderer.messageAndPos(cont.contained(), cont.pos, messageRenderer.diagnosticLevel(cont))(_)

  /** Output errors to `out` */
  private def displayErrors(errs: Seq[MessageContainer])(implicit state: State): State = {
    errs.map(renderMessage(_)(state.run.runContext)).foreach(out.println)
    state
  }
}
