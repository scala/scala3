package dotty.tools.repl

import dotty.tools.dotc.ast.Trees._
import dotty.tools.dotc.ast.{tpd, untpd}
import dotty.tools.dotc.ast.tpd.TreeOps
import dotty.tools.dotc.core.Comments.CommentsContext
import dotty.tools.dotc.core.Contexts._
import dotty.tools.dotc.core.Decorators._
import dotty.tools.dotc.core.Flags._
import dotty.tools.dotc.core.Names._
import dotty.tools.dotc.core.Phases.Phase
import dotty.tools.dotc.core.StdNames._
import dotty.tools.dotc.core.Symbols._
import dotty.tools.dotc.reporting.Diagnostic
import dotty.tools.dotc.transform.{PostTyper, Staging}
import dotty.tools.dotc.typer.ImportInfo._
import dotty.tools.dotc.typer.TyperPhase
import dotty.tools.dotc.util.Spans._
import dotty.tools.dotc.util.{ParsedComment, SourceFile}
import dotty.tools.dotc.{CompilationUnit, Compiler, Run}
import dotty.tools.repl.results._

import scala.collection.mutable

/** This subclass of `Compiler` is adapted for use in the REPL.
 *
 *  - compiles parsed expression in the current REPL state:
 *    - adds the appropriate imports in scope
 *    - wraps expressions into a dummy object
 *  - provides utility to query the type of an expression
 *  - provides utility to query the documentation of an expression
 */
class ReplCompiler extends Compiler {

  override protected def frontendPhases: List[List[Phase]] = List(
    List(new TyperPhase(addRootImports = false)),
    List(new CollectTopLevelImports),
    List(new PostTyper),
  )

  def newRun(initCtx: Context, state: State): Run = new Run(this, initCtx) {

    /** Import previous runs and user defined imports */
    override protected def rootContext(using Context): Context = {
      def importContext(imp: tpd.Import)(using Context) =
        ctx.importContext(imp, imp.symbol)

      def importPreviousRun(id: Int)(using Context) = {
        // we first import the wrapper object id
        val path = nme.REPL_PACKAGE ++ "." ++ objectNames(id)
        val ctx0 = ctx.fresh
          .setNewScope
          .withRootImports(RootRef(() => requiredModuleRef(path)) :: Nil)

        // then its user defined imports
        val imports = state.imports.getOrElse(id, Nil)
        if imports.isEmpty then ctx0
        else imports.foldLeft(ctx0.fresh.setNewScope)((ctx, imp) =>
          importContext(imp)(using ctx))
      }

      val rootCtx = super.rootContext
        .withRootImports   // default root imports
        .withRootImports(RootRef(() => defn.EmptyPackageVal.termRef) :: Nil)
      (1 to state.objectIndex).foldLeft(rootCtx)((ctx, id) =>
        importPreviousRun(id)(using ctx))
    }
  }

  private val objectNames = mutable.Map.empty[Int, TermName]

  private case class Definitions(stats: List[untpd.Tree], state: State)

  private def definitions(trees: List[untpd.Tree], state: State): Definitions = inContext(state.context) {
    import untpd._

    // If trees is of the form `{ def1; def2; def3 }` then `List(def1, def2, def3)`
    val flattened = trees match {
      case List(Block(stats, expr)) =>
        if (expr eq EmptyTree) stats // happens when expr is not an expression
        else stats :+ expr
      case _ =>
        trees
    }

    var valIdx = state.valIndex
    val defs = new mutable.ListBuffer[Tree]

    flattened.foreach {
      case expr @ Assign(id: Ident, _) =>
        // special case simple reassignment (e.g. x = 3)
        // in order to print the new value in the REPL
        val assignName = (id.name ++ str.REPL_ASSIGN_SUFFIX).toTermName
        val assign = ValDef(assignName, TypeTree(), id).withSpan(expr.span)
        defs += expr += assign
      case expr if expr.isTerm =>
        val resName = (str.REPL_RES_PREFIX + valIdx).toTermName
        valIdx += 1
        val vd = ValDef(resName, TypeTree(), expr).withSpan(expr.span)
        defs += vd
      case other =>
        defs += other
    }

    Definitions(
      defs.toList,
      state.copy(
        objectIndex = state.objectIndex + 1,
        valIndex = valIdx
      )
    )
  }

  /** Wrap trees in an object and add imports from the previous compilations
   *
   *  The resulting structure is something like:
   *
   *  ```
   *  package <none> {
   *    object rs$line$nextId {
   *      import rs$line${i <- 0 until nextId}._
   *
   *      <trees>
   *    }
   *  }
   *  ```
   */
  private def wrapped(defs: Definitions, objectTermName: TermName, span: Span): untpd.PackageDef =
    inContext(defs.state.context) {
      import untpd._

      val tmpl = Template(emptyConstructor, Nil, Nil, EmptyValDef, defs.stats)
      val module = ModuleDef(objectTermName, tmpl)
        .withSpan(span)

      PackageDef(Ident(nme.REPL_PACKAGE), List(module))
    }

  private def createUnit(defs: Definitions, span: Span)(using Context): CompilationUnit = {
    val objectName = ctx.source.file.toString
    assert(objectName.startsWith(str.REPL_SESSION_LINE))
    assert(objectName.endsWith(defs.state.objectIndex.toString))
    val objectTermName = ctx.source.file.toString.toTermName
    objectNames.update(defs.state.objectIndex, objectTermName)

    val unit = new ReplCompilationUnit(ctx.source)
    unit.untpdTree = wrapped(defs, objectTermName, span)
    unit
  }

  private def runCompilationUnit(unit: CompilationUnit, state: State): Result[(CompilationUnit, State)] = {
    val ctx = state.context
    ctx.run.compileUnits(unit :: Nil)

    if (!ctx.reporter.hasErrors) (unit, state).result
    else ctx.reporter.removeBufferedMessages(using ctx).errors
  }

  final def compile(parsed: Parsed)(implicit state: State): Result[(CompilationUnit, State)] = {
    assert(!parsed.trees.isEmpty)
    val defs = definitions(parsed.trees, state)
    val unit = createUnit(defs, Span(0, parsed.trees.last.span.end))(using state.context)
    runCompilationUnit(unit, defs.state)
  }

  final def typeOf(expr: String)(implicit state: State): Result[String] =
    typeCheck(expr).map { tree =>
      given Context = state.context
      tree.rhs match {
        case Block(xs, _) => xs.last.tpe.widen.show
        case _ =>
          """Couldn't compute the type of your expression, so sorry :(
            |
            |Please report this to my masters at github.com/lampepfl/dotty
          """.stripMargin
      }
    }

  def docOf(expr: String)(implicit state: State): Result[String] = inContext(state.context) {

    /** Extract the "selected" symbol from `tree`.
     *
     *  Because the REPL typechecks an expression, special syntax is needed to get the documentation
     *  of certain symbols:
     *
     *  - To select the documentation of classes, the user needs to pass a call to the class' constructor
     *    (e.g. `new Foo` to select `class Foo`)
     *  - When methods are overloaded, the user needs to enter a lambda to specify which functions he wants
     *    (e.g. `foo(_: Int)` to select `def foo(x: Int)` instead of `def foo(x: String)`
     *
     *  This function returns the right symbol for the received expression, and all the symbols that are
     *  overridden.
     */
    def extractSymbols(tree: tpd.Tree): Iterator[Symbol] = {
      val sym = tree match {
        case tree if tree.isInstantiation => tree.symbol.owner
        case tpd.closureDef(defdef) => defdef.rhs.symbol
        case _ => tree.symbol
      }
      Iterator(sym) ++ sym.allOverriddenSymbols
    }

    typeCheck(expr).map {
      case ValDef(_, _, Block(stats, _)) if stats.nonEmpty =>
        val stat = stats.last.asInstanceOf[tpd.Tree]
        if (stat.tpe.isError) stat.tpe.show
        else {
          val symbols = extractSymbols(stat)
          val doc = for {
            sym <- symbols
            comment <- ParsedComment.docOf(sym)
          } yield comment.renderAsMarkdown

          if (doc.hasNext) doc.next()
          else s"// No doc for `$expr`"
        }

      case _ =>
        """Couldn't display the documentation for your expression, so sorry :(
          |
          |Please report this to my masters at github.com/lampepfl/dotty
          """.stripMargin
    }
  }

  final def typeCheck(expr: String, errorsAllowed: Boolean = false)(implicit state: State): Result[tpd.ValDef] = {

    def wrapped(expr: String, sourceFile: SourceFile, state: State)(using Context): Result[untpd.PackageDef] = {
      def wrap(trees: List[untpd.Tree]): untpd.PackageDef = {
        import untpd._

        val valdef = ValDef("expr".toTermName, TypeTree(), Block(trees, unitLiteral).withSpan(Span(0, expr.length)))
        val tmpl = Template(emptyConstructor, Nil, Nil, EmptyValDef, List(valdef))
        val wrapper = TypeDef("$wrapper".toTypeName, tmpl)
          .withMods(Modifiers(Final))
          .withSpan(Span(0, expr.length))
        PackageDef(Ident(nme.REPL_PACKAGE), List(wrapper))
      }

      ParseResult(sourceFile)(state) match {
        case Parsed(_, trees, _) =>
          wrap(trees).result
        case SyntaxErrors(_, reported, trees) =>
          if (errorsAllowed) wrap(trees).result
          else reported.errors
        case _ => List(
          new Diagnostic.Error(
            s"Couldn't parse '$expr' to valid scala",
            sourceFile.atSpan(Span(0, expr.length))
          )
        ).errors
      }
    }

    def unwrapped(tree: tpd.Tree, sourceFile: SourceFile)(using Context): Result[tpd.ValDef] = {
      def error: Result[tpd.ValDef] =
        List(new Diagnostic.Error(s"Invalid scala expression",
          sourceFile.atSpan(Span(0, sourceFile.content.length)))).errors

      import tpd._
      tree match {
        case PackageDef(_, List(TypeDef(_, tmpl: Template))) =>
          tmpl.body
              .collectFirst { case dd: ValDef if dd.name.show == "expr" => dd.result }
              .getOrElse(error)
        case _ =>
          error
      }
    }


    val src = SourceFile.virtual("<typecheck>", expr)
    inContext(state.context.fresh
      .setReporter(newStoreReporter)
      .setSetting(state.context.settings.YstopAfter, List("typer"))
    ) {
      wrapped(expr, src, state).flatMap { pkg =>
        val unit = CompilationUnit(src)
        unit.untpdTree = pkg
        ctx.run.compileUnits(unit :: Nil, ctx)

        if (errorsAllowed || !ctx.reporter.hasErrors)
          unwrapped(unit.tpdTree, src)
        else
          ctx.reporter.removeBufferedMessages.errors
      }
    }
  }
}
