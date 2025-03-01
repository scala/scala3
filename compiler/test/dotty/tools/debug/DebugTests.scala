package dotty.tools.debug

import com.sun.jdi.*
import dotty.Properties
import dotty.tools.dotc.reporting.TestReporter
import dotty.tools.io.JFile
import dotty.tools.vulpix.*
import org.junit.Test

import java.util.concurrent.TimeoutException
import scala.concurrent.duration.*
import scala.util.control.NonFatal

class DebugTests:
  import DebugTests.*
  @Test def debug: Unit =
    implicit val testGroup: TestGroup = TestGroup("debug")
    // compileFile("tests/debug/eval-local-class-in-value-class.scala", TestConfiguration.defaultOptions).checkDebug()
    compileFilesInDir("tests/debug", TestConfiguration.defaultOptions).checkDebug()

object DebugTests extends ParallelTesting:
  def maxDuration =
    // Increase the timeout when the user is debugging the tests
    if isUserDebugging then 3.hours else 45.seconds
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
        val checkFile = testSource.checkFile.getOrElse(throw new Exception("Missing check file")).toPath
        val debugSteps = DebugStepAssert.parseCheckFile(checkFile)
        val expressionEvaluator =
          ExpressionEvaluator(testSource.sourceFiles, testSource.flags, testSource.runClassPath, testSource.outDir)
        try debugMain(testSource.runClassPath): debuggee =>
          val jdiPort = debuggee.readJdiPort()
          val debugger = Debugger(jdiPort, expressionEvaluator, maxDuration/* , verbose = true */)
          // configure the breakpoints before starting the debuggee
          val breakpoints = debugSteps.map(_.step).collect { case b: DebugStep.Break => b }.distinct
          for b <- breakpoints do debugger.configureBreakpoint(b.className, b.line)
          try
            debuggee.launch()
            playDebugSteps(debugger, debugSteps/* , verbose = true */)
            val status = debuggee.exit()
            reportDebuggeeStatus(testSource, status)
          finally
            // closing the debugger must be done at the very end so that the
            // 'Listening for transport dt_socket at address: <port>' message is ready to be read
            // by the next DebugTest
            debugger.dispose()
        catch case DebugStepException(message, location) =>
          echo(s"\n[error] Debug step failed: $location\n" + message)
          failTestSource(testSource)
    end verifyDebug

    private def playDebugSteps(debugger: Debugger, steps: Seq[DebugStepAssert[?]], verbose: Boolean = false): Unit =
      import scala.language.unsafeNulls

      /** The DebugTests can only debug one thread at a time. It cannot handle breakpoints in concurrent threads.
       *  When thread is null, it means the JVM is running and no thread is waiting to be resumed.
       *  If thread is not null, it is waiting to be resumed by calling continue, step or next.
       *  While the thread is paused, it can be used for evaluation.
       */
      var thread: ThreadReference = null
      def location = thread.frame(0).location
      def continueIfPaused(): Unit =
        if thread != null then
          debugger.continue(thread)
          thread = null

      for case step <- steps do
        import DebugStep.*
        try step match
          case DebugStepAssert(Break(className, line), assert) =>
            continueIfPaused()
            thread = debugger.break()
            if verbose then
              println(s"break $location ${location.method.name}")
            assert(location)
          case DebugStepAssert(Next, assert) =>
            thread = debugger.next(thread)
            if verbose then println(s"next $location ${location.method.name}")
            assert(location)
          case DebugStepAssert(Step, assert) =>
            thread = debugger.step(thread)
            if verbose then println(s"step $location ${location.method.name}")
            assert(location)
          case DebugStepAssert(Eval(expr), assert) =>
            if verbose then println(s"eval $expr")
            val result = debugger.evaluate(expr, thread)
            if verbose then println(result.fold("error " + _, "result " + _))
            assert(result)
        catch
          case _: TimeoutException => throw new DebugStepException("Timeout", step.location)
          case e: DebugStepException => throw e
          case NonFatal(e) =>
            throw new Exception(s"Debug step failed unexpectedly: ${step.location}", e)
      end for
      // let the debuggee finish its execution
      continueIfPaused()
    end playDebugSteps

    private def reportDebuggeeStatus(testSource: TestSource, status: Status): Unit =
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
  end DebugTest
