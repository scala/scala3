package dotty.tools
package repl

import dotc.ast.{ untpd, tpd }
import dotc.{ CompilationUnit, Compiler }
import dotc.util.SourceFile
import dotc.core.Decorators._
import dotc.core.Flags._
import dotc.core.Contexts.Context
import dotc.reporting._
import dotc.reporting.diagnostic._
import dotc.util.Positions.Position
import dotc.interfaces.Diagnostic.ERROR

import results._

class ReplTyper(ictx: Context) {

  private val compiler = new Compiler {
    override def phases = List(new REPLFrontEnd :: Nil)
  }

  private def wrapped(expr: String, sourceFile: SourceFile, nextId: Int)(implicit ctx: Context): Result[untpd.PackageDef] = {
    def wrap(trees: Seq[untpd.Tree]): untpd.PackageDef = {
      import untpd._
      import dotc.core.StdNames._

      val imports = List.range(0, nextId).map { i =>
        Import(Ident(("ReplSession$" + i).toTermName), Ident(nme.WILDCARD) :: Nil)
      }

      val valdef =
        ValDef("expr".toTermName, TypeTree(), Block(trees.init.toList, trees.last))

      val tmpl = Template(emptyConstructor, Nil, EmptyValDef, imports :+ valdef)
      PackageDef(Ident(nme.NO_NAME),
        ModuleDef("EvaluateExpr".toTermName, tmpl)
          .withMods(new Modifiers(Module | Final))
          .withPos(Position(trees.head.pos.start, trees.last.pos.end)) :: Nil
      )
    }

    ParseResult(expr) match {
      case Parsed(sourceCode, trees) =>
        wrap(trees).result
      case SyntaxErrors(reported, _) =>
        reported.errors
      case _ => Seq(
        new messages.Error(
          s"Couldn't parse '$expr' to valid scala",
          sourceFile.atPos(Position(0, expr.length))
        )
      ).errors
    }
  }

  private def extractTpe(tree: tpd.Tree, sourceFile: SourceFile)(implicit ctx: Context): Result[String] = {

    def error: Result[String] =
      Seq(new messages.Error(s"Invalid scala expression",
        sourceFile.atPos(Position(0, sourceFile.content.length)))).errors

    import tpd._
    tree match {
      case PackageDef(_, List(_, TypeDef(_, tmpl: Template))) =>
        tmpl.body
          .collect { case vd: ValDef => vd }
          .find(_.name.show == "expr")
          .map(_.symbol.info.show.result)
          .getOrElse(error)
      case _ =>
        error
    }
  }

  def typeOf(expr: String, state: State): Result[String] = {
    implicit val ctx = ictx

    val reporter = new StoreReporter(null) with UniqueMessagePositions with HideNonSensicalMessages
    val run  = compiler.newRun(ctx.fresh.setReporter(reporter))
    val src  = new SourceFile(s"EvaluateExpr", expr)
    val unit = new CompilationUnit(src)

    wrapped(expr, src, state.objectIndex).flatMap { pkg =>
      unit.untpdTree = pkg
      run.compileUnits(unit :: Nil)
      val errs = reporter.removeBufferedMessages
      if (errs.isEmpty) extractTpe(unit.tpdTree, src)(run.runContext)
      else errs.errors
    }
  }

}
