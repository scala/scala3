package dotty.partest

import scala.tools.partest.{ TestState, nest }
import java.io.File


/* NOTE: Adapted from partest.DirectCompiler and DottyTest */
class DPDirectCompiler(runner: nest.Runner) extends nest.DirectCompiler(runner) {

  override def compile(opts0: List[String], sources: List[File]): TestState = {
    println("\ncompiling " + sources.mkString(" ") + "\noptions: " + opts0.mkString(" ")) 

    implicit var ctx: dotty.tools.dotc.core.Contexts.Context = {
      val base = new dotty.tools.dotc.core.Contexts.ContextBase
      import base.settings._
      val ctx = base.initialCtx.fresh.setSetting(printtypes, true)
        .setSetting(pageWidth, 90).setSetting(log, List("<some"))
      base.definitions.init(ctx)
      ctx
    }
    
    try {
      val processor = if (opts0.exists(_.startsWith("#"))) dotty.tools.dotc.Bench else dotty.tools.dotc.Main
      val reporter = processor.process((sources.map(_.toString) ::: opts0).toArray, ctx)
      if (!reporter.hasErrors) runner.genPass()
      else {
        reporter.printSummary(ctx)
        runner.genFail(s"compilation failed with ${reporter.errorCount} errors")
      }
    } catch { 
      case t: Throwable => runner.genCrash(t)
    }
  }
}
