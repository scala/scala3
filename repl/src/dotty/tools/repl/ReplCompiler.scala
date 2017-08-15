package dotty.tools
package repl

import java.io.{ File => JFile }

import dotc.ast.{ untpd, tpd }
import dotc.{ Run, CompilationUnit, Compiler }
import dotc.core.Decorators._, dotc.core.Flags._, dotc.core.Phases
import dotc.core.Names._, dotc.core.Contexts._, dotc.core.StdNames.nme
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

  sealed case class Definitions(stats: List[untpd.Tree], state: State)

  def definitions(trees: List[untpd.Tree], state: State)(implicit ctx: Context): Result[Definitions] = {
    import untpd._

    def createShow(name: TermName, pos: Position) = {
      val showName = name ++ "Show"
      val select = Select(Ident(name), "show".toTermName)
      ValDef(showName, TypeTree(), select).withFlags(Synthetic).withPos(pos)
    }

    val (exps, other) = trees.partition(_.isTerm)
    val resX = exps.zipWithIndex.flatMap { (exp, i) =>
      val resName = s"res${i + state.valIndex}".toTermName
      val show = createShow(resName, exp.pos)

      exp match {
        case exp @ Assign(id: Ident, rhs) =>
          val assignName = (id.name ++ "$Assign").toTermName
          val assign = ValDef(assignName, TypeTree(), id).withPos(exp.pos)

          exp :: assign :: createShow(assignName, exp.pos) :: Nil
        case _ =>
          List(ValDef(resName, TypeTree(), exp).withPos(exp.pos), show)
      }
    }

    val othersWithShow = other.flatMap {
      case t: ValDef =>
        List(t, createShow(t.name, t.pos))
      case t: PatDef => {
        val variables = t.pats.flatMap {
          case Ident(name) if name != nme.WILDCARD => List((name.toTermName, t.pos))
          case pat =>
            new UntypedDeepFolder[List[(TermName, Position)]](
            (acc: List[(TermName, Position)], t: Tree) => t match {
              case _: BackquotedIdent =>
                acc
              case Ident(name) if name.isVariableName && name.toString != "_" =>
                (name.toTermName, t.pos) :: acc
              case Bind(name, _) if name.isVariableName =>
                (name.toTermName, t.pos) :: acc
              case _ =>
                acc
            }
          ).apply(Nil, pat).reverse
        }

        t :: variables.map(createShow)
      }
      case t => List(t)
    }

    val newObjectIndex = state.objectIndex + {
      if (resX.nonEmpty || othersWithShow.nonEmpty) 1 else 0
    }
    val newValIndex = state.valIndex + exps.filterNot(_.isInstanceOf[Assign]).length

    Definitions(
      state.imports.map(_._1) ++ resX ++ othersWithShow,
      state.copy(objectIndex = newObjectIndex, valIndex = newValIndex)
    ).result
  }

  /** Wrap trees in an object and add imports from the previous compilations
   *
   *  The resulting structure is something like:
   *
   *  ```
   *  package <none> {
   *    object ReplSession$nextId {
   *      import ReplSession${i <- 0 until nextId}._
   *      import dotty.Show._
   *
   *      <trees>
   *    }
   *  }
   *  ```
   */
  def wrapped(defs: Definitions)(implicit ctx: Context): untpd.PackageDef = {
    import untpd._
    import dotc.core.StdNames._

    val module = {
      val tmpl = Template(emptyConstructor(ctx), Nil, EmptyValDef, defs.stats)
      List(
        ModuleDef(s"ReplSession$$${ defs.state.objectIndex }".toTermName, tmpl)
        .withMods(new Modifiers(Module | Final))
        .withPos(Position(defs.stats.head.pos.start, defs.stats.last.pos.end))
      )
    }

    PackageDef(Ident(nme.EMPTY_PACKAGE), module)
  }

  def newRun(state: State, initCtx: Context, reporter: Reporter) = new Run(this, initCtx) {
    override protected[this] def rootContext(implicit ctx: Context) =
      addMagicImports(super.rootContext, state).fresh.setReporter(reporter)
  }

  def createUnit(defs: Definitions, sourceCode: String)(implicit ctx: Context): Result[CompilationUnit] = {
    val unit = new CompilationUnit(new SourceFile(s"ReplsSession$$${ defs.state.objectIndex }", sourceCode))
    unit.untpdTree = wrapped(defs)
    unit.result
  }

  def runCompilation(unit: CompilationUnit, state: State, initCtx: Context): Result[State] = {
    val reporter = storeReporter

    val run = newRun(state, initCtx, reporter)
    run.compileUnits(unit :: Nil)

    if (!reporter.hasErrors)
      state.copy(ctx = run.runContext).result
    else
      reporter.removeBufferedMessages(state.ctx).errors
  }

  def compile(parsed: Parsed, state: State, initCtx: Context): Result[(CompilationUnit, State)] = {
    for {
      defs  <- definitions(parsed.trees, state)(initCtx)
      unit  <- createUnit(defs, parsed.sourceCode)(initCtx)
      state <- runCompilation(unit, defs.state, initCtx)
    } yield (unit, state)
  }

  private[this] def addMagicImports(initCtx: Context, state: State): Context = {
    def addImport(path: TermName)(implicit ctx: Context) = {
      val ref = tpd.ref(ctx.requiredModuleRef(path.toTermName))
      val symbol = ctx.newImportSymbol(ctx.owner, ref)
      val importInfo =
        new ImportInfo(implicit ctx => symbol, untpd.Ident(nme.WILDCARD) :: Nil, None)
      ctx.fresh.setNewScope.setImportInfo(importInfo)
    }

    List
      .range(1, state.objectIndex + 1)
      .foldLeft(addImport("dotty.Show".toTermName)(initCtx)) { (ictx, i) =>
        addImport(nme.EMPTY_PACKAGE ++ (".ReplSession$" + i))(ictx)
      }
  }

  def typeOf(expr: String, state: State, rootCtx: Context): Result[String] =
    typeCheck(expr, state, rootCtx).map { (tree, ictx) =>
      import dotc.ast.Trees._
      implicit val ctx = ictx
      tree.rhs match {
        case Block(xs, _) => xs.last.tpe.widen.show
        case _ =>
          """Couldn't compute the type of your expression, so sorry :(
            |
            |Please report this to my masters at Github.com/lampepfl/dotty
          """.stripMargin
      }
    }

  def typeCheck(expr: String, state: State, rootCtx: Context, errorsAllowed: Boolean = false): Result[(tpd.ValDef, Context)] = {

    def wrapped(expr: String, sourceFile: SourceFile, state: State)(implicit ctx: Context): Result[untpd.PackageDef] = {
      def wrap(trees: Seq[untpd.Tree]): untpd.PackageDef = {
        import untpd._

        val valdef =
          ValDef("expr".toTermName, TypeTree(), Block(trees.toList, untpd.unitLiteral))

        val tmpl =
          Template(emptyConstructor, Ident("Any".toTypeName) :: Nil, EmptyValDef, state.imports.map(_._1) :+ valdef)

        PackageDef(Ident(nme.EMPTY_PACKAGE),
          TypeDef("EvaluateExpr".toTypeName, tmpl)
            .withMods(new Modifiers(Final))
            .withPos(Position(trees.head.pos.start, trees.last.pos.end)) :: Nil
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


    val reporter = storeReporter
    val run = newRun(state, rootCtx.fresh.setSetting(rootCtx.settings.YstopAfter, List("replFrontEnd")), reporter)
    val src = new SourceFile(s"EvaluateExpr", expr)
    val runCtx = run.runContext

    wrapped(expr, src, state)(runCtx).flatMap { pkg =>
      val unit = new CompilationUnit(src)
      unit.untpdTree = pkg
      run.compileUnits(unit :: Nil)

      if (errorsAllowed || !reporter.hasErrors)
        unwrapped(unit.tpdTree, src)(runCtx).map((_, runCtx))
      else {
        reporter.removeBufferedMessages(runCtx).errors
      }
    }
  }
}
