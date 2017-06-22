package dotty.tools
package repl

import java.io.{ File => JFile }

import dotc.ast.{ untpd, tpd }
import dotc.{ CompilationUnit, Compiler }
import dotc.core.{ Phases, Decorators, Flags }
import Decorators._, Flags._
import dotc.util.SourceFile
import dotc.typer.FrontEnd
import backend.jvm.GenBCode
import dotc.core.Contexts.Context
import dotc.util.Positions._
import dotc.reporting.diagnostic.{ messages, MessageContainer }
import dotc.reporting._
import io._

import results._

class ReplCompiler(ictx: Context) extends Compiler {

  type NextRes = Int

  /** Directory to save class files to */
  final val virtualDirectory =
    if (ictx.settings.d.isDefault(ictx))
      new VirtualDirectory("(memory)", None)
    else
      new PlainDirectory(new Directory(new JFile(ictx.settings.d.value(ictx))))


  /** A GenBCode phase that outputs to a virtual directory */
  private class REPLGenBCode extends GenBCode {
    override def phaseName = "replGenBCode"
    override def outputDir(implicit ctx: Context) = virtualDirectory
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

  sealed case class Definitions(trees: Seq[untpd.Tree], state: State)

  def definitions(trees: Seq[untpd.Tree], state: State): Result[Definitions] = {
    import untpd._
    implicit val ctx = ictx

    def freeExpression(t: Tree) =
      t.isTerm && !t.isInstanceOf[Assign]

    val (exps, other) = trees.partition(freeExpression)
    val resX = exps.zipWithIndex.flatMap { (exp, i) =>
      val resName = s"res${i + state.valIndex}".toTermName
      val showName = resName ++ "Show"
      val showApply = Apply(Select(Ident(resName), "show".toTermName), Nil)
      List(
        ValDef(resName, TypeTree(), exp).withPos(exp.pos),
        ValDef(showName, TypeTree(), showApply).withPos(exp.pos).withFlags(Synthetic)
      )
    }

    val othersWithShow = other.flatMap {
      case t: ValDef => {
        val tShow =
          cpy.ValDef(t)(name = t.name ++ "Show", rhs = Select(Ident(t.name), "show".toTermName))
            .withFlags(Synthetic)

        List(t, tShow)
      }
      case t => List(t)
    }

    Definitions(
      resX ++ othersWithShow,
      state.copy(valIndex = state.valIndex + exps.length)
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
  def wrapped(trees: Seq[untpd.Tree], nextId: Int)(implicit ctx: Context): untpd.PackageDef = {
    import untpd._
    import dotc.core.StdNames._

    val imports =
      Import(Ident("dotty".toTermName), Ident("Show".toTermName) :: Nil) ::
      Import(Ident("Show".toTermName), Ident(nme.WILDCARD) :: Nil) ::
      List.range(0, nextId).map { i =>
        Import(Ident(("ReplSession$" + i).toTermName), Ident(nme.WILDCARD) :: Nil)
      }

    val tmpl = Template(emptyConstructor, Nil, EmptyValDef, imports ++ trees)
    PackageDef(Ident(nme.NO_NAME),
      ModuleDef(s"ReplSession$$$nextId".toTermName, tmpl)
        .withMods(new Modifiers(Module | Final))
        .withPos(Position(trees.head.pos.start, trees.last.pos.end)) :: Nil
    )
  }

  def createUnit(trees: Seq[untpd.Tree], objectIndex: Int, sourceCode: String)(implicit ctx: Context): Result[CompilationUnit] = {
    val unit = new CompilationUnit(new SourceFile(s"ReplsSession$$$objectIndex", sourceCode))
    unit.untpdTree = wrapped(trees, objectIndex)
    unit.result
  }

  def runCompilation(unit: CompilationUnit, state: State): Result[(State, Context)] = {
    implicit val ctx = ictx
    val reporter = new StoreReporter(null) with UniqueMessagePositions with HideNonSensicalMessages
    val run = newRun(ctx.fresh.setReporter(reporter))
    run.compileUnits(unit :: Nil)

    val errs = reporter.removeBufferedMessages
    if (errs.isEmpty) {
      val newState = State(state.objectIndex + 1, state.valIndex, state.history)
      (newState, run.runContext).result
    }
    else errs.errors
  }

  def compile(parsed: Parsed, state: State): Result[(CompilationUnit, State, Context)] = {
    implicit val ctx = ictx
    for {
      defs         <- definitions(parsed.trees, state)
      unit         <- createUnit(defs.trees, state.objectIndex, parsed.sourceCode)
      (state, ctx) <- runCompilation(unit, defs.state)
    } yield (unit, state, ctx)
  }


  def typeOf(expr: String, state: State): Result[String] = {

    def extractTpe(tree: tpd.Tree, sourceFile: SourceFile)(implicit ctx: Context): Result[String] = {
      def error: Result[String] =
        Seq(new messages.Error(s"Invalid scala expression",
          sourceFile.atPos(Position(0, sourceFile.content.length)))).errors

      import tpd._
      tree match {
        case PackageDef(_, List(TypeDef(_,tmpl: Template))) =>
          tmpl.body
            .collect { case vd: ValDef => vd }
            .find(_.name.show == "expr")
            .map(_.symbol.info.show.result)
            .getOrElse(error)

        case _ =>
          error
      }
    }

    def wrapped(expr: String, sourceFile: SourceFile, nextId: Int)(implicit ctx: Context): Result[untpd.PackageDef] = {
      def wrap(trees: Seq[untpd.Tree]): untpd.PackageDef = {
        import untpd._
        import dotc.core.StdNames._

        val imports = List.range(0, nextId).map { i =>
          Import(Ident(("ReplSession$" + i).toTermName), Ident(nme.WILDCARD) :: Nil)
        }

        val valdef =
          ValDef("expr".toTermName, TypeTree(), Block(trees.init.toList, trees.last))

        val tmpl = Template(emptyConstructor, Ident("Any".toTypeName) :: Nil, EmptyValDef, imports :+ valdef)
        PackageDef(Ident(nme.NO_NAME),
          TypeDef("EvaluateExpr".toTypeName, tmpl)
            .withMods(new Modifiers(Final))
            .withPos(Position(trees.head.pos.start, trees.last.pos.end)) :: Nil
        )
      }

      ParseResult(expr) match {
        case Parsed(sourceCode, trees) =>
          wrap(trees).result
        case SyntaxErrors(reported) =>
          reported.errors
        case _ => Seq(
          new messages.Error(
            s"Couldn't parse '$expr' to valid scala",
            sourceFile.atPos(Position(0, expr.length))
          )
        ).errors
      }
    }

    implicit val ctx: Context = ictx.fresh.setSetting(ictx.settings.YstopAfter, List("frontEnd"))

    val reporter = new StoreReporter(null) with UniqueMessagePositions with HideNonSensicalMessages
    val src  = new SourceFile(s"EvaluateExpr", expr)
    val unit = new CompilationUnit(src)

    wrapped(expr, src, state.objectIndex).flatMap { pkg =>
      unit.untpdTree = pkg
      val run  = newRun(ctx.fresh.setReporter(reporter))
      run.compileUnits(unit :: Nil)
      val errs = reporter.removeBufferedMessages
      if (errs.isEmpty) extractTpe(unit.tpdTree, src)(run.runContext)
      else errs.errors
    }
  }
}
