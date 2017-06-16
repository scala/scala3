package dotty.tools
package repl

import java.io.{ File => JFile }

import dotc.ast.untpd
import dotc.ast.tpd
import dotc.{ CompilationUnit, Compiler }
import dotc.core.{ Phases, Decorators, Flags }
import Decorators._, Flags._
import dotc.util.SourceFile
import dotc.typer.FrontEnd
import backend.jvm.GenBCode
import dotc.core.Contexts.Context
import dotc.util.Positions._
import dotc.reporting.diagnostic.MessageContainer
import dotc.reporting._
import io._

import results._

class ReplCompiler(ictx: Context) extends Compiler {

  type NextRes = Int

  private class REPLFrontEnd extends FrontEnd {
    override def phaseName = "replFrontEnd"

    override def runOn(units: List[CompilationUnit])(implicit ctx: Context) = {
      val unitContexts = for (unit <- units) yield ctx.fresh.setCompilationUnit(unit)
      var remaining = unitContexts
      while (remaining.nonEmpty) {
        enterSyms(remaining.head)
        remaining = remaining.tail
      }
      unitContexts.foreach(enterAnnotations(_))
      unitContexts.foreach(typeCheck(_))
      unitContexts.map(_.compilationUnit).filterNot(discardAfterTyper)
    }
  }

  /** A GenBCode phase that outputs to a virtual directory */
  private class REPLGenBCode extends GenBCode {
    override def phaseName = "replGenBCode"

    /** Directory to save class files to */
    private val virtualDirectory =
      if (ictx.settings.d.isDefault(ictx))
        new VirtualDirectory("(memory)", None)
      else
        new PlainDirectory(new Directory(new JFile(ictx.settings.d.value(ictx))))

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

  def freeToAssigned(trees: Seq[untpd.Tree], state: State)
                    (implicit ctx: Context): (State, Seq[untpd.Tree]) = {
    import untpd._

    def freeExpression(t: Tree) =
      t.isTerm && !t.isInstanceOf[Assign]

    val (exps, other) = trees.partition(freeExpression)
    val resX = exps.zipWithIndex.map { (exp, i) =>
      ValDef(s"res${i + state.freeValues}".toTermName, TypeTree(), exp)
        .withPos(exp.pos)
    }

    (state.copy(freeValues = state.freeValues + resX.length), resX ++ other)
  }

  def wrapped(trees: Seq[untpd.Tree], nextId: Int)(implicit ctx: Context): untpd.PackageDef = {
    import untpd._
    import dotc.core.StdNames._

    val imports = List.range(0, nextId).map{ i =>
      Import(Ident(("ReplSession$" + i).toTermName), Ident(nme.WILDCARD) :: Nil)
    }

    val tmpl = Template(emptyConstructor, Nil, EmptyValDef, imports ++ trees)
    PackageDef(Ident(nme.NO_NAME),
      ModuleDef(("ReplSession$" + nextId).toTermName, tmpl)
        .withMods(new Modifiers(Module | Final))
        .withPos(Position(trees.head.pos.start, trees.last.pos.end)) :: Nil
    )
  }

  def compile(parsed: Parsed, state: State)(implicit ctx: Context): Result[State] = {
    val reporter = new StoreReporter(null) with UniqueMessagePositions with HideNonSensicalMessages

    val unit = new CompilationUnit(new SourceFile("ReplSession$" + (state.objects + 1), parsed.sourceCode))
    val (stateAfterAssign, trees) = freeToAssigned(parsed.trees, state)
    unit.untpdTree = wrapped(trees, 0)

    val run = newRun(ctx.fresh.setReporter(reporter))
    run.compileUnits(unit :: Nil)

    val errs = reporter.removeBufferedMessages
    if (errs.isEmpty) state.copy(
      freeValues = stateAfterAssign.freeValues,
      objects = state.objects + 1,
      ictx = run.runContext
    )
    else Errors(errs)
  }
}
