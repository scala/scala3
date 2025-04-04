package dotty.tools.debug

import com.sun.jdi.*
import dotty.Properties
import dotty.tools.dotc.reporting.TestReporter
import dotty.tools.io.JFile
import dotty.tools.vulpix.*
import org.junit.AfterClass
import org.junit.Test

import java.util.concurrent.TimeoutException
import scala.concurrent.duration.*
import scala.util.control.NonFatal

class DebugTests:
  import DebugTests.*
  @Test def debug: Unit =
    implicit val testGroup: TestGroup = TestGroup("debug")
    CompilationTest.aggregateTests(
      compileFile("tests/debug-custom-args/eval-explicit-nulls.scala", TestConfiguration.explicitNullsOptions),
      compileFilesInDir("tests/debug", TestConfiguration.defaultOptions),
      compileFilesInDir("tests/debug-preview", TestConfiguration.defaultOptions.and("-preview"))
    ).checkDebug()

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
  @AfterClass def tearDown(): Unit =
    super.cleanup()
    summaryReport.echoSummary()

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
      /** The DebugTests can only debug one thread at a time. It cannot handle breakpoints in concurrent threads.
       *  When thread is None, it means the JVM is running and no thread is waiting to be resumed.
       *  If thread is Some, it is waiting to be resumed by calling continue, step or next.
       *  While the thread is paused, it can be used for evaluation.
       */
      var thread: Option[ThreadReference] = None
      def location = thread.get.frame(0).location
      def continueIfPaused(): Unit =
        thread.foreach(debugger.continue)
        thread = None

      for case step <- steps do
        import DebugStep.*
        try step match
          case DebugStepAssert(Break(className, line), assertion) =>
            continueIfPaused()
            thread = Some(debugger.break())
            if verbose then
              println(s"break $location ${location.method.name}")
            assertion(location)
          case DebugStepAssert(Next, assertion) =>
            thread = Some(debugger.next(thread.get))
            if verbose then println(s"next $location ${location.method.name}")
            assertion(location)
          case DebugStepAssert(Step, assertion) =>
            thread = Some(debugger.step(thread.get))
            if verbose then println(s"step $location ${location.method.name}")
            assertion(location)
          case DebugStepAssert(Eval(expr), assertion) =>
            if verbose then println(s"eval $expr")
            val result = debugger.evaluate(expr, thread.get)
            if verbose then println(result.fold("error " + _, "result " + _))
            assertion(result)
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
