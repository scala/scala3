package dotty.tools
package dotc
package decompiler

import dotty.tools.dotc.core.Contexts._
import dotty.tools.dotc.core._
import dotty.tools.dotc.core.tasty.TastyHTMLPrinter
import dotty.tools.dotc.reporting._
import dotty.tools.dotc.tastyreflect.ReflectionImpl

/**
  * Decompiler to be used with IDEs
  */
class IDEDecompilerDriver(val settings: List[String]) extends dotc.Driver {

  private val myInitCtx: Context = {
    val rootCtx = initCtx.fresh.addMode(Mode.Interactive).addMode(Mode.ReadPositions).addMode(Mode.ReadComments)
    rootCtx.setSetting(rootCtx.settings.YretainTrees, true)
    rootCtx.setSetting(rootCtx.settings.fromTasty, true)
    val ctx = setup(settings.toArray :+ "dummy.scala", rootCtx)._2
    ctx.initialize()(ctx)
    ctx
  }

  private val decompiler = new PartialTASTYDecompiler

  def run(className: String): (String, String) = {
    val reporter = new StoreReporter(null) with HideNonSensicalMessages

    val run = decompiler.newRun(myInitCtx.fresh.setReporter(reporter))

    implicit val ctx = run.runContext

    run.compile(List(className))
    run.printSummary()
    val unit = ctx.run.units.head

    val decompiled = ReflectionImpl.showTree(unit.tpdTree)
    val tree = new TastyHTMLPrinter(unit.pickled.head._2).printContents()

    reporter.removeBufferedMessages.foreach(message => System.err.println(message))
    (tree, decompiled)
  }
}
