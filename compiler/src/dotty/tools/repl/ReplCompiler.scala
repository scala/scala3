package dotty.tools
package repl

import java.io.{ File => JFile }

import dotc.ast.Trees._
import dotc.ast.{ untpd, tpd }
import dotc.{ Run, CompilationUnit, Compiler }
import dotc.core.Decorators._, dotc.core.Flags._, dotc.core.Phases
import dotc.core.Names._, dotc.core.Contexts._, dotc.core.StdNames._
import dotc.core.Constants.Constant
import dotc.util.SourceFile
import dotc.typer.{ ImportInfo, FrontEnd }
import backend.jvm.GenBCode
import dotc.core.NameOps._
import dotc.util.Positions._
import dotc.reporting.diagnostic.{ messages, MessageContainer }
import dotc.reporting._
import io._

import results._

/** This subclass of `Compiler` replaces the appropriate phases in order to
 *  facilitate the REPL
 *
 *  Specifically it replaces the front end with `REPLFrontEnd`, and adds a
 *  custom subclass of `GenBCode`. The custom `GenBCode`, `REPLGenBCode`, works
 *  in conjunction with a specialized class loader in order to load virtual
 *  classfiles.
 */
class ReplCompiler(val directory: AbstractFile) extends Compiler {


  /** A GenBCode phase that outputs to a virtual directory */
  private class REPLGenBCode extends GenBCode {
    override def phaseName = "replGenBCode"
    override def outputDir(implicit ctx: Context) = directory
  }

  override def phases = {
    val replacedFrontend = Phases.replace(
      classOf[FrontEnd],
      _ => new REPLFrontEnd :: Nil,
      super.phases
    )

    Phases.replace(
      classOf[GenBCode],
      _ => new REPLGenBCode :: Nil,
      replacedFrontend
    )
  }

  private[this] var objectNames = Map.empty[Int, TermName]
  private def objectName(state: State) =
    objectNames.get(state.objectIndex).getOrElse {
      val newName = (str.REPL_SESSION_LINE + state.objectIndex).toTermName
      objectNames = objectNames + (state.objectIndex -> newName)
      newName
    }

  sealed case class Definitions(stats: List[untpd.Tree], state: State)

  def definitions(trees: List[untpd.Tree], state: State): Result[Definitions] = {
    import untpd._

    implicit val ctx: Context = state.run.runContext

    def createShow(name: TermName, pos: Position) = {
      val showName = name ++ "Show"
      val select = Select(Ident(name), "show".toTermName)
      val valAsAnyRef = TypeApply(Select(Ident(name), nme.asInstanceOf_),
                                  List(Ident(tpnme.AnyRef)))
      val cond = InfixOp(valAsAnyRef,
                         Ident(nme.EQ),
                         Literal(Constant(null)))
      val showWithNullCheck = If(cond, Literal(Constant("null")), select)
      DefDef(showName, Nil, Nil, TypeTree(), showWithNullCheck).withFlags(Synthetic).withPos(pos)
    }

    def createPatDefShows(patDef: PatDef) = {
      def createDeepShows(tree: untpd.Tree) = {
        object PatFolder extends UntypedDeepFolder[List[DefDef]] (
          (acc, tree) => tree match {
            case Ident(name) if name.isVariableName && name != nme.WILDCARD =>
              createShow(name.toTermName, tree.pos) :: acc
            case Bind(name, _) if name.isVariableName && name != nme.WILDCARD =>
              createShow(name.toTermName, tree.pos) :: acc
            case _ =>
              acc
          }
        )
        PatFolder.apply(Nil, tree).reverse
      }

      // cannot fold over the whole tree because we need to generate show methods
      // for top level identifier starting with an uppercase (e.g. val X, Y = 2)
      patDef.pats.flatMap {
        case id @ Ident(name) if name != nme.WILDCARD =>
          List(createShow(name.toTermName, id.pos))
        case bd @ Bind(name, body) if name != nme.WILDCARD =>
          createShow(name.toTermName, bd.pos) :: createDeepShows(body)
        case other =>
          createDeepShows(other)
      }
    }

    var valIdx = state.valIndex

    val defs = trees.flatMap {
      case vd: ValDef =>
        List(vd, createShow(vd.name, vd.pos))
      case pd: PatDef =>
        pd :: createPatDefShows(pd)
      case expr @ Assign(id: Ident, rhs) =>
        // special case simple reassignment (e.g. x = 3)
        // in order to print the new value in the REPL
        val assignName = (id.name ++ str.REPL_ASSIGN_SUFFIX).toTermName
        val assign = ValDef(assignName, TypeTree(), id).withPos(expr.pos)
        val show = createShow(assignName, expr.pos)
        List(expr, assign, show)
      case expr if expr.isTerm =>
        val resName = (str.REPL_RES_PREFIX + valIdx).toTermName
        valIdx += 1
        val show = createShow(resName, expr.pos)
        val vd = ValDef(resName, TypeTree(), expr).withPos(expr.pos)
        List(vd, show)
      case other =>
        List(other)
    }

    Definitions(
      state.imports.map(_._1) ++ defs,
      state.copy(
        objectIndex = state.objectIndex + (if (defs.isEmpty) 0 else 1),
        valIndex = valIdx
      )
    ).result
  }

  /** Wrap trees in an object and add imports from the previous compilations
   *
   *  The resulting structure is something like:
   *
   *  ```
   *  package <none> {
   *    object rs$line$nextId {
   *      import rs$line${i <- 0 until nextId}._
   *      import dotty.Show._
   *
   *      <trees>
   *    }
   *  }
   *  ```
   */
  def wrapped(defs: Definitions, sourceCode: String): untpd.PackageDef = {
    import untpd._

    implicit val ctx: Context = defs.state.run.runContext

    val module = {
      val tmpl = Template(emptyConstructor(ctx), Nil, EmptyValDef, defs.stats)
      List(
        ModuleDef(objectName(defs.state), tmpl)
          .withMods(new Modifiers(Module | Final))
          .withPos(Position(0, sourceCode.length))
      )
    }

    PackageDef(Ident(nme.EMPTY_PACKAGE), module)
  }

  def newRun(initCtx: Context, objectIndex: Int) = new Run(this, initCtx) {
    override protected[this] def rootContext(implicit ctx: Context) =
      addMagicImports(super.rootContext.fresh.setReporter(storeReporter), objectIndex)
  }

  def createUnit(defs: Definitions, sourceCode: String): Result[CompilationUnit] = {
    val unit = new CompilationUnit(new SourceFile(objectName(defs.state).toString, sourceCode))
    unit.untpdTree = wrapped(defs, sourceCode)
    unit.result
  }

  def runCompilation(unit: CompilationUnit, state: State): Result[State] = {
    val run = state.run
    val reporter = state.run.runContext.reporter
    run.compileUnits(unit :: Nil)

    if (!reporter.hasErrors) state.result
    else run.runContext.flushBufferedMessages().errors
  }

  def compile(parsed: Parsed)(implicit state: State): Result[(CompilationUnit, State)] = {
    for {
      defs  <- definitions(parsed.trees, state)
      unit  <- createUnit(defs, parsed.sourceCode)
      state <- runCompilation(unit, defs.state)
    } yield (unit, state)
  }

  private[this] def addMagicImports(initCtx: Context, objectIndex: Int): Context = {
    def addImport(path: TermName)(implicit ctx: Context) = {
      val ref = tpd.ref(ctx.requiredModuleRef(path.toTermName))
      val symbol = ctx.newImportSymbol(ctx.owner, ref)
      val importInfo =
        new ImportInfo(implicit ctx => symbol, untpd.Ident(nme.WILDCARD) :: Nil, None)
      ctx.fresh.setNewScope.setImportInfo(importInfo)
    }

    List
      .range(1, objectIndex + 1)
      .foldLeft(addImport("dotty.Show".toTermName)(initCtx)) { (ictx, i) =>
        addImport(nme.EMPTY_PACKAGE ++ "." ++ objectNames(i))(ictx)
      }
  }

  def typeOf(expr: String)(implicit state: State): Result[String] =
    typeCheck(expr).map { tree =>
      import dotc.ast.Trees._
      implicit val ctx = state.run.runContext
      tree.rhs match {
        case Block(xs, _) => xs.last.tpe.widen.show
        case _ =>
          """Couldn't compute the type of your expression, so sorry :(
            |
            |Please report this to my masters at Github.com/lampepfl/dotty
          """.stripMargin
      }
    }

  def typeCheck(expr: String, errorsAllowed: Boolean = false)(implicit state: State): Result[tpd.ValDef] = {

    def wrapped(expr: String, sourceFile: SourceFile, state: State)(implicit ctx: Context): Result[untpd.PackageDef] = {
      def wrap(trees: Seq[untpd.Tree]): untpd.PackageDef = {
        import untpd._

        val valdef =
          ValDef("expr".toTermName, TypeTree(), Block(trees.toList, untpd.unitLiteral))

        val tmpl = Template(emptyConstructor,
                            List(Ident(tpnme.Any)),
                            EmptyValDef,
                            state.imports.map(_._1) :+ valdef)

        PackageDef(Ident(nme.EMPTY_PACKAGE),
          TypeDef("EvaluateExpr".toTypeName, tmpl)
            .withMods(new Modifiers(Final))
            .withPos(Position(0, expr.length)) :: Nil
        )
      }

      ParseResult(expr) match {
        case Parsed(sourceCode, trees) =>
          wrap(trees).result
        case SyntaxErrors(_, reported, trees) =>
          if (errorsAllowed) wrap(trees).result
          else reported.errors
        case _ => List(
          new messages.Error(
            s"Couldn't parse '$expr' to valid scala",
            sourceFile.atPos(Position(0, expr.length))
          )
        ).errors
      }
    }

    def unwrapped(tree: tpd.Tree, sourceFile: SourceFile)(implicit ctx: Context): Result[tpd.ValDef] = {
      def error: Result[tpd.ValDef] =
        List(new messages.Error(s"Invalid scala expression",
          sourceFile.atPos(Position(0, sourceFile.content.length)))).errors

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


    val run = state.run
    val reporter = state.run.runContext.reporter
    val src = new SourceFile(s"EvaluateExpr", expr)
    val runCtx =
      run.runContext.fresh
         .setSetting(run.runContext.settings.YstopAfter, List("replFrontEnd"))

    wrapped(expr, src, state)(runCtx).flatMap { pkg =>
      val unit = new CompilationUnit(src)
      unit.untpdTree = pkg
      run.compileUnits(unit :: Nil, runCtx.fresh.setReporter(storeReporter))

      if (errorsAllowed || !reporter.hasErrors)
        unwrapped(unit.tpdTree, src)(runCtx)
      else {
        reporter.removeBufferedMessages(runCtx).errors
      }
    }
  }
}
