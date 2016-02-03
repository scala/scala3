package dotty.partest

import dotty.tools.dotc.reporting.ConsoleReporter
import scala.tools.partest.{ TestState, nest }
import java.io.{ File, PrintWriter, FileWriter }


/* NOTE: Adapted from partest.DirectCompiler */
class DPDirectCompiler(runner: DPTestRunner) extends nest.DirectCompiler(runner) {

  override def compile(opts0: List[String], sources: List[File]): TestState = {
    val clogFWriter = new FileWriter(runner.cLogFile.jfile, true)
    val clogWriter = new PrintWriter(clogFWriter, true)
    clogWriter.println("\ncompiling " + sources.mkString(" ") + "\noptions: " + opts0.mkString(" "))

    try {
      val processor =
        if (opts0.exists(_.startsWith("#"))) dotty.tools.dotc.Bench else dotty.tools.dotc.Main
      val clogger = new ConsoleReporter(writer = clogWriter)
      val reporter = processor.process((sources.map(_.toString) ::: opts0).toArray, clogger)
      if (!reporter.hasErrors) runner.genPass()
      else {
        clogWriter.println(reporter.summary)
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
