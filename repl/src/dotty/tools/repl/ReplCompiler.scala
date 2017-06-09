package dotty.tools
package repl

import java.io.{ File => JFile }

import dotc.ast.untpd
import dotc.{ CompilationUnit, Compiler }
import dotc.core.{ Phases, Decorators, Flags }
import Decorators._, Flags._
import dotc.util.SourceFile
import dotc.typer.FrontEnd
import backend.jvm.GenBCode
import dotc.core.Contexts.Context
import dotc.reporting._
import io._

class ReplCompiler(ictx: Context) extends Compiler {

  ictx.base.initialize()(ictx)

  type NextRes = Int

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

  private class REPLFrontEnd extends FrontEnd {
    override def phaseName = "replFrontEnd"

    override def runOn(units: List[CompilationUnit])(implicit ctx: Context) = {
      for (unit <- units) do println(unit.untpdTree.show)

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

  override def phases = {
    val replPhases = Phases.replace(classOf[FrontEnd], _ => new REPLFrontEnd :: Nil, super.phases)
    Phases.replace(classOf[GenBCode], _ => new REPLGenBCode :: Nil, replPhases)
  }

  def wrapped(trees: Seq[untpd.Tree], currentRes: Int)(implicit ctx: Context): (NextRes, untpd.ModuleDef) = {
    import untpd._
    def freeExpression(t: Tree) =
      t.isTerm && !t.isInstanceOf[Assign]

    val (exps, other) = trees.partition(freeExpression)

    val resX = exps.zipWithIndex.map { (exp, i) =>
      ValDef(s"res${i + currentRes}".toTermName, TypeTree(), exp)
    }

    val moduleIndex = resX.length + currentRes
    val module = {
      val stats = if (trees.isEmpty) List(EmptyTree) else resX ++ other
      val tmpl = Template(emptyConstructor, Nil, EmptyValDef, stats)
      val moduleName = s"EncapsulatedRes$moduleIndex".toTermName

      ModuleDef(moduleName, tmpl).withFlags(Module)
    }

    (moduleIndex + 1, module)
  }

  def compile(parsed: Parsed, currentRes: Int)(implicit ctx: Context): NextRes = {
    val reporter = new StoreReporter(null) with UniqueMessagePositions with HideNonSensicalMessages

    val unit = new CompilationUnit(new SourceFile(s"repl-run-$currentRes", parsed.sourceCode))
    val (newRes, tree) = wrapped(parsed.trees, currentRes)
    unit.untpdTree = tree

    val run = newRun(ctx.fresh.setReporter(reporter))
    run.compileUnits(unit :: Nil)

    newRes
  }
}
