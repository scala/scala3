package dotty.partest

import dotty.tools.dotc.reporting.ConsoleReporter
import scala.tools.partest.{ TestState, nest }
import java.io.{ File, PrintWriter, FileWriter }


/* NOTE: Adapted from partest.DirectCompiler and DottyTest */
class DPDirectCompiler(runner: DPTestRunner) extends nest.DirectCompiler(runner) {

  override def compile(opts0: List[String], sources: List[File]): TestState = {
    val clogFWriter = new FileWriter(runner.cLogFile.jfile, true)
    val clogWriter = new PrintWriter(clogFWriter, true)
    clogWriter.println("\ncompiling " + sources.mkString(" ") + "\noptions: " + opts0.mkString(" "))

    implicit val ctx: dotty.tools.dotc.core.Contexts.Context = {
      val base = new dotty.tools.dotc.core.Contexts.ContextBase
      import base.settings._
      val ctx = base.initialCtx.fresh.setSetting(printtypes, true)
        .setSetting(pageWidth, 90).setSetting(log, List("<some"))
      base.definitions.init(ctx)
      ctx
    }

    try {
      val processor =
        if (opts0.exists(_.startsWith("#"))) dotty.tools.dotc.Bench else dotty.tools.dotc.Main
      val clogger = new ConsoleReporter(writer = clogWriter)(ctx)
      val logCtx = ctx.fresh.setTyperState(ctx.typerState.withReporter(clogger))
      val reporter = processor.process((sources.map(_.toString) ::: opts0).toArray, logCtx)
      if (!reporter.hasErrors) runner.genPass()
      else {
        reporter.printSummary(ctx)
        runner.genFail(s"compilation failed with ${reporter.errorCount} errors")
      }
    } catch {
      case t: Throwable =>
        t.printStackTrace
        t.printStackTrace(clogWriter)
        runner.genCrash(t)
    } finally {
      clogFWriter.close
      clogWriter.close
    }
  }
}
