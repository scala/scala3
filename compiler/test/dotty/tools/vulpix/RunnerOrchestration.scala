package dotty
package tools
package vulpix

import scala.language.unsafeNulls

import java.io.{ File => JFile, InputStreamReader, IOException, BufferedReader, PrintStream }
import java.nio.file.Paths
import java.nio.charset.StandardCharsets
import java.util.concurrent.atomic.AtomicBoolean
import java.util.concurrent.TimeoutException

import scala.concurrent.duration.Duration
import scala.concurrent.{ Await, Future }
import scala.concurrent.ExecutionContext.Implicits.global
import scala.collection.mutable
import scala.compiletime.uninitialized

/** Vulpix spawns JVM subprocesses (`numberOfSlaves`) in order to run tests
 *  without compromising the main JVM
 *
 *  These need to be orchestrated in a safe manner with a simple protocol. This
 *  interface provides just that.
 *
 *  The protocol is defined as:
 *
 *  - master sends classpath to for which to run `Test#main` and waits for
 *    `maxDuration`
 *  - slave invokes the method and waits until completion
 *  - upon completion it sends back a `RunComplete` message
 *  - the master checks if the child is still alive
 *    - child is still alive, the output was valid
 *    - child is dead, the output is the failure message
 *
 *  If this whole chain of events is not completed within `maxDuration`, the
 *  child process is destroyed and a new child is spawned.
 */
trait RunnerOrchestration {

  /** The maximum amount of active runners, which contain a child JVM */
  def numberOfSlaves: Int

  /** The maximum duration the child process is allowed to consume before
   *  getting destroyed
   */
  def maxDuration: Duration

  /** Destroy and respawn process after each test */
  def safeMode: Boolean

  /** Open JDI connection for testing the debugger */
  def debugMode: Boolean = false

  /** Running a `Test` class's main method from the specified `classpath` */
  def runMain(classPath: String, toolArgs: ToolArgs)(implicit summaryReport: SummaryReporting): Status =
    monitor.runMain(classPath)

  /** Each method of Debuggee can be called only once, in the order of definition.*/
  trait Debuggee:
    /** read the jdi port to connect the debugger */
    def readJdiPort(): Int
    /** start the main method in the background */
    def launch(): Unit
    /** wait until the end of the main method */
    def exit(): Status

  /** Provide a Debuggee for debugging the Test class's main method
   *  @param f the debugging flow: set breakpoints, launch main class, pause, step, evaluate, exit etc
   */
  def debugMain(classPath: String)(f: Debuggee => Unit)(implicit summaryReport: SummaryReporting): Unit =
    assert(debugMode, "debugMode is disabled")
    monitor.debugMain(classPath)(f)

  /** Kill all processes */
  def cleanup() = monitor.killAll()

  private val monitor = new RunnerMonitor

  /** The runner monitor object keeps track of child JVM processes by keeping
   *  them in two structures - one for free, and one for busy children.
   *
   *  When a user calls `runMain` the monitor makes takes a free JVM and blocks
   *  until the run is complete - or `maxDuration` has passed. It then performs
   *  cleanup by returning the used JVM to the free list, or respawning it if
   *  it died
   */
  private class RunnerMonitor {

    def runMain(classPath: String)(implicit summaryReport: SummaryReporting): Status =
      withRunner(_.runMain(classPath))

    def debugMain(classPath: String)(f: Debuggee => Unit)(implicit summaryReport: SummaryReporting): Unit =
      withRunner(_.debugMain(classPath)(f))

    private class RunnerProcess(p: Process):
      private val stdout = new BufferedReader(new InputStreamReader(p.getInputStream(), StandardCharsets.UTF_8))
      private val stdin = new PrintStream(p.getOutputStream(), /* autoFlush = */ true)

      def readLine(): String =
        stdout.readLine() match
          case s"Listening for transport dt_socket at address: $port" =>
            throw new IOException(
              s"Unexpected transport dt_socket message." +
              " The port is going to be lost and no debugger will be able to connect."
            )
          case line => line

      def printLine(line: String): Unit = stdin.println(line)

      def getJdiPort(): Int =
        stdout.readLine() match
          case s"Listening for transport dt_socket at address: $port" => port.toInt
          case line => throw new IOException(s"Failed getting JDI port of child JVM: got $line")

      export p.{exitValue, isAlive, destroy}
    end RunnerProcess

    private class Runner(private var process: RunnerProcess):
      /** Checks if `process` is still alive
       *
       *  When `process.exitValue()` is called on an active process the caught
       *  exception is thrown. As such we can know if the subprocess exited or
       *  not.
       */
      def isAlive: Boolean =
        try { process.exitValue(); false }
        catch case _: IllegalThreadStateException => true

      /** Destroys the underlying process and kills IO streams */
      def kill(): Unit =
        if (process ne null) process.destroy()
        process = null

      /** Blocks less than `maxDuration` while running `Test.main` from `dir` */
      def runMain(classPath: String): Status =
        assert(process ne null, "Runner was killed and then reused without setting a new process")
        awaitStatusOrRespawn(startMain(classPath))

      def debugMain(classPath: String)(f: Debuggee => Unit): Unit =
        assert(process ne null, "Runner was killed and then reused without setting a new process")

        val debuggee = new Debuggee:
          private var mainFuture: Future[Status] = null
          def readJdiPort(): Int = process.getJdiPort()
          def launch(): Unit = mainFuture = startMain(classPath)
          def exit(): Status =
            awaitStatusOrRespawn(mainFuture)

        try f(debuggee)
        catch case e: Throwable =>
          // if debugging failed it is safer to respawn a new process
          respawn()
          throw e
      end debugMain

      private def startMain(classPath: String): Future[Status] =
        // pass classpath to running process
        process.printLine(classPath)

        // Create a future reading the object:
        Future:
          val sb = new StringBuilder

          var childOutput: String = process.readLine()

          // Discard all messages until the test starts
          while (childOutput != ChildJVMMain.MessageStart && childOutput != null)
            childOutput = process.readLine()
          childOutput = process.readLine()

          while childOutput != ChildJVMMain.MessageEnd && childOutput != null do
            sb.append(childOutput).append(System.lineSeparator)
            childOutput = process.readLine()

          if process.isAlive() && childOutput != null then Success(sb.toString)
          else Failure(sb.toString)
      end startMain

      // wait status of the main class execution, respawn if failure or timeout
      private def awaitStatusOrRespawn(future: Future[Status]): Status =
        val status =
          try Await.result(future, maxDuration)
          catch case _: TimeoutException => Timeout
        // handle failures
        status match
          case _: Success if !safeMode => () // no need to respawn
          case _ => respawn() // safeMode, failure or timeout
        status

      // Makes the encapsulating RunnerMonitor spawn a new runner
      private def respawn(): Unit =
        process.destroy()
        process = null
        process = createProcess()
    end Runner

    /** Create a process which has the classpath of the `ChildJVMMain` and the
     *  scala library.
     */
    private def createProcess(): RunnerProcess =
      val url = classOf[ChildJVMMain].getProtectionDomain.getCodeSource.getLocation
      val cp = Paths.get(url.toURI).toString + JFile.pathSeparator + Properties.scalaLibrary
      val javaBin = Paths.get(sys.props("java.home"), "bin", "java").toString
      val args = Seq("-Dfile.encoding=UTF-8", "-Duser.language=en", "-Duser.country=US", "-Xmx1g", "-cp", cp) ++
        (if debugMode then Seq("-agentlib:jdwp=transport=dt_socket,server=y,suspend=n,quiet=n") else Seq.empty)
      val command = (javaBin +: args) :+ "dotty.tools.vulpix.ChildJVMMain"
      val process = new ProcessBuilder(command*)
        .redirectErrorStream(true)
        .redirectInput(ProcessBuilder.Redirect.PIPE)
        .redirectOutput(ProcessBuilder.Redirect.PIPE)
        .start()
      RunnerProcess(process)

    private val freeRunners = mutable.Queue.empty[Runner]
    private val busyRunners = mutable.Set.empty[Runner]

    private def getRunner(): Runner = synchronized {
      while (freeRunners.isEmpty && busyRunners.size >= numberOfSlaves) wait()

      val runner =
        if (freeRunners.isEmpty) new Runner(createProcess())
        else freeRunners.dequeue()
      busyRunners += runner

      notify()
      runner
    }

    private def freeRunner(runner: Runner): Unit = synchronized {
      freeRunners.enqueue(runner)
      busyRunners -= runner
      notify()
    }

    private def withRunner[T](op: Runner => T)(using summaryReport: SummaryReporting): T =
      val runner = getRunner()
      val result = op(runner)
      freeRunner(runner)
      result

    def killAll(): Unit = {
      freeRunners.foreach(_.kill())
      busyRunners.foreach(_.kill())
    }

    // On shutdown, we need to kill all runners:
    sys.addShutdownHook(killAll())
  }
}
