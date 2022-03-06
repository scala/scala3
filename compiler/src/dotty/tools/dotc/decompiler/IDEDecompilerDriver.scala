package dotty.tools
package dotc
package decompiler

import scala.language.unsafeNulls

import dotty.tools.dotc.core.Contexts._
import dotty.tools.dotc.core._
import dotty.tools.dotc.core.tasty.TastyHTMLPrinter
import dotty.tools.dotc.reporting._
import dotty.tools.io.AbstractFile

import scala.quoted.runtime.impl.QuotesImpl

/**
  * Decompiler to be used with IDEs
  */
class IDEDecompilerDriver(val settings: List[String]) extends dotc.Driver {

  private val myInitCtx: Context = {
    val rootCtx = initCtx.fresh.addMode(Mode.Interactive | Mode.ReadPositions)
    rootCtx.setSetting(rootCtx.settings.YreadComments, true)
    rootCtx.setSetting(rootCtx.settings.YretainTrees, true)
    rootCtx.setSetting(rootCtx.settings.fromTasty, true)
    val ctx = setup(settings.toArray :+ "dummy.scala", rootCtx).get._2
    ctx.initialize()(using ctx)
    ctx
  }

  private val decompiler = new PartialTASTYDecompiler

  def run(tastyFile: AbstractFile): (String, String) = {
    val reporter = new StoreReporter(null) with HideNonSensicalMessages

    val run = decompiler.newRun(using myInitCtx.fresh.setReporter(reporter))

    inContext(run.runContext) {
      run.compile(List(tastyFile))
      run.printSummary()
      val unit = ctx.run.nn.units.head

      val decompiled = QuotesImpl.showDecompiledTree(unit.tpdTree)
      val tree = new TastyHTMLPrinter(unit.pickled.head._2()).showContents()

      reporter.removeBufferedMessages.foreach(message => System.err.println(message))
      (tree, decompiled)
    }
  }
}
