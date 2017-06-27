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

  def definitions(trees: Seq[untpd.Tree], state: State)(implicit ctx: Context): Result[Definitions] = {
    import untpd._

    def freeExpression(t: Tree) =
      t.isTerm && !t.isInstanceOf[Assign]

    val (exps, other) = trees.partition(freeExpression)
    val show = "show".toTermName
    val resX = exps.zipWithIndex.flatMap { (exp, i) =>
      val resName = s"res${i + state.valIndex}".toTermName
      val showName = resName ++ "Show"
      val showApply = Apply(Select(Ident(resName), show), Nil)
      List(
        ValDef(resName, TypeTree(), exp).withPos(exp.pos),
        ValDef(showName, TypeTree(), showApply).withPos(exp.pos).withFlags(Synthetic)
      )
    }

    val othersWithShow = other.flatMap {
      case t: ValDef => {
        val name = t.name ++ "Show"
        val select = Select(Ident(t.name), show)
        List(t, ValDef(name, TypeTree(), select).withFlags(Synthetic))
      }
      case t => List(t)
    }

    Definitions(
      state.imports.map(_._1) ++ resX ++ othersWithShow,
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
  def wrapped(trees: Seq[untpd.Tree], state: State)(implicit ctx: Context): untpd.PackageDef = {
    import untpd._
    import dotc.core.StdNames._

    val tmpl = Template(emptyConstructor(ctx), Nil, EmptyValDef, trees)
    PackageDef(Ident(nme.NO_NAME),
      ModuleDef(s"ReplSession$$${ state.objectIndex }".toTermName, tmpl)
        .withMods(new Modifiers(Module | Final))
        .withPos(Position(trees.head.pos.start, trees.last.pos.end)) :: Nil
    )
  }

  def createUnit(trees: Seq[untpd.Tree], state: State, sourceCode: String)(implicit ctx: Context): Result[CompilationUnit] = {
    val unit = new CompilationUnit(new SourceFile(s"ReplsSession$$${ state.objectIndex }", sourceCode))
    unit.untpdTree = wrapped(trees, state)
    unit.result
  }

  def runCompilation(unit: CompilationUnit, state: State)(implicit ctx: Context): Result[(State, Context)] = {
    val reporter = new StoreReporter(null) with UniqueMessagePositions with HideNonSensicalMessages
    val run = new Run(this)(ctx.fresh.setReporter(reporter))
    run.compileUnits(unit :: Nil)

    if (!reporter.hasErrors)
      (state.copy(objectIndex = state.objectIndex + 1), run.runContext).result
    else
      reporter.removeBufferedMessages.errors
  }

  def compile(parsed: Parsed, state: State): Result[(CompilationUnit, State, Context)] = {
    implicit val ctx: Context = addMagicImports(state)(rootContext(ictx))
    for {
      defs          <- definitions(parsed.trees, state)
      unit          <- createUnit(defs.trees, state, parsed.sourceCode)
      (state, ictx) <- runCompilation(unit, defs.state)
    } yield (unit, state, ictx)
  }

  def addMagicImports(state: State)(implicit ctx: Context): Context = {
    def addImport(path: TermName)(implicit ctx: Context) = {
      val ref = tpd.ref(ctx.requiredModuleRef(path.toTermName))
      val symbol = ctx.newImportSymbol(ctx.owner, ref)
      val importInfo =
        new ImportInfo(implicit ctx => symbol, untpd.Ident(nme.WILDCARD) :: Nil, None)
      ctx.fresh.setNewScope.setImportInfo(importInfo)
    }

    List
      .range(0, state.objectIndex)
      .foldLeft(addImport("dotty.Show".toTermName)) { (ictx, i) =>
        addImport(nme.NO_NAME ++ (".ReplSession$" + i))(ictx)
      }
  }

  def typeOf(expr: String, state: State): Result[String] = {

    def extractTpe(tree: tpd.Tree, sourceFile: SourceFile)(implicit ctx: Context): Result[String] = {
      def error: Result[String] =
        List(new messages.Error(s"Invalid scala expression",
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

    def wrapped(expr: String, sourceFile: SourceFile, state: State)(implicit ctx: Context): Result[untpd.PackageDef] = {
      def wrap(trees: Seq[untpd.Tree]): untpd.PackageDef = {
        import untpd._

        val valdef =
          ValDef("expr".toTermName, TypeTree(), Block(trees.init.toList, trees.last))

        val tmpl =
          Template(emptyConstructor, Ident("Any".toTypeName) :: Nil, EmptyValDef, state.imports.map(_._1) :+ valdef)

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
        case _ => List(
          new messages.Error(
            s"Couldn't parse '$expr' to valid scala",
            sourceFile.atPos(Position(0, expr.length))
          )
        ).errors
      }
    }

    implicit val ctx: Context =
      addMagicImports(state)(rootContext(ictx))
      .fresh.setSetting(ictx.settings.YstopAfter, List("frontEnd"))

    val reporter = new StoreReporter(null) with UniqueMessagePositions with HideNonSensicalMessages
    val src  = new SourceFile(s"EvaluateExpr", expr)

    wrapped(expr, src, state).flatMap { pkg =>
      val unit = new CompilationUnit(src)
      unit.untpdTree = pkg
      val run  = new Run(this)(ctx.fresh.setReporter(reporter))
      run.compileUnits(unit :: Nil)
      if (!reporter.hasErrors) extractTpe(unit.tpdTree, src)(run.runContext)
      else reporter.removeBufferedMessages.errors
    }
  }
}
