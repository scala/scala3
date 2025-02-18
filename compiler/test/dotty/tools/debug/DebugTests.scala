package dotty.tools.debug

import com.sun.jdi.*
import dotty.Properties
import dotty.tools.dotc.reporting.TestReporter
import dotty.tools.io.JFile
import dotty.tools.vulpix.*
import org.junit.Test

import scala.concurrent.duration.*

class DebugTests:
  import DebugTests.*
  @Test def debug: Unit =
    implicit val testGroup: TestGroup = TestGroup("debug")
    // compileFile("tests/debug/for.scala", TestConfiguration.defaultOptions).checkDebug()
    compileFilesInDir("tests/debug", TestConfiguration.defaultOptions).checkDebug()

object DebugTests extends ParallelTesting:
  def maxDuration = 45.seconds
  def numberOfSlaves = Runtime.getRuntime().availableProcessors()
  def safeMode = Properties.testsSafeMode
  def isInteractive = SummaryReport.isInteractive
  def testFilter = Properties.testsFilter
  def updateCheckFiles: Boolean = Properties.testsUpdateCheckfile
  def failedTests = TestReporter.lastRunFailedTests
  override def debugMode = true

  implicit val summaryReport: SummaryReporting = new SummaryReport

  extension (test: CompilationTest)
    private def checkDebug()(implicit summaryReport: SummaryReporting): test.type =
      import test.*
      checkPass(new DebugTest(targets, times, threadLimit, shouldFail || shouldSuppressOutput), "Debug")

  private final class DebugTest(testSources: List[TestSource], times: Int, threadLimit: Option[Int], suppressAllOutput: Boolean)(implicit summaryReport: SummaryReporting)
    extends RunTest(testSources, times, threadLimit, suppressAllOutput):

    override def onSuccess(testSource: TestSource, reporters: Seq[TestReporter], logger: LoggedRunnable) =
      verifyDebug(testSource.outDir, testSource, countWarnings(reporters), reporters, logger)

    private def verifyDebug(dir: JFile, testSource: TestSource, warnings: Int, reporters: Seq[TestReporter], logger: LoggedRunnable) =
      if Properties.testsNoRun then addNoRunWarning()
      else
        val checkFile = testSource.checkFile.getOrElse(throw new Exception("Missing check file"))
        val debugSteps = DebugStepAssert.parseCheckFile(checkFile)
        val expressionEvaluator = ExpressionEvaluator(testSource.sourceFiles, testSource.flags, testSource.runClassPath, testSource.outDir)
        val status = debugMain(testSource.runClassPath): debuggee =>
          val debugger = Debugger(debuggee.jdiPort, expressionEvaluator, maxDuration/* , verbose = true */)
          // configure the breakpoints before starting the debuggee
          val breakpoints = debugSteps.map(_.step).collect { case b: DebugStep.Break => b }
          for b <- breakpoints do debugger.configureBreakpoint(b.className, b.line)
          try
            debuggee.launch()
            playDebugSteps(debugger, debugSteps/* , verbose = true */)
          finally
            // stop debugger to let debuggee terminate its execution
            debugger.dispose()
        status match
          case Success(output) => ()
          case Failure(output) =>
            if output == "" then
              echo(s"Test '${testSource.title}' failed with no output")
            else
              echo(s"Test '${testSource.title}' failed with output:")
              echo(output)
            failTestSource(testSource)
          case Timeout =>
            echo("failed because test " + testSource.title + " timed out")
            failTestSource(testSource, TimeoutFailure(testSource.title))
    end verifyDebug

    private def playDebugSteps(debugger: Debugger, steps: Seq[DebugStepAssert[?]], verbose: Boolean = false): Unit =
      /** The DebugTests can only debug one thread at a time. It cannot handle breakpoints in concurrent threads.
       *  When thread is None, it means the JVM is running and no thread is waiting to be resumed.
       *  If thread is Some, it is waiting to be resumed by calling continue, step or next.
       *  While the thread is paused, it can be used for evaluation.
       */
      var thread: ThreadReference = null
      def location = thread.frame(0).location

      for case step <- steps do
        import DebugStep.*
        step match
          case DebugStepAssert(Break(className, line), assert) =>
            // continue if paused
            if thread != null then
              debugger.continue(thread)
              thread = null
            thread = debugger.break()
            if verbose then println(s"break ${location.declaringType.name} ${location.lineNumber}")
            assert(location)
          case DebugStepAssert(Next, assert) =>
            thread = debugger.next(thread)
            if verbose then println(s"next ${location.lineNumber}")
            assert(location)
          case DebugStepAssert(Step, assert) =>
            thread = debugger.step(thread)
            if verbose then println(s"step ${location.lineNumber}")
            assert(location)
          case DebugStepAssert(Eval(expr), assert) =>
            val result = debugger.evaluate(expr, thread)
            if verbose then println(s"eval $expr $result")
            assert(result)
    end playDebugSteps
  end DebugTest
