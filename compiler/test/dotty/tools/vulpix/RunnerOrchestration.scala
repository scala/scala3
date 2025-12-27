package dotty
package tools
package vulpix

import java.io.{File as JFile, InputStreamReader, IOException, BufferedReader, PrintStream}
import java.nio.file.Paths
import java.nio.charset.StandardCharsets.UTF_8
import java.util.concurrent.TimeoutException

import scala.concurrent.duration.Duration
import scala.concurrent.{Await, Future}
import scala.concurrent.ExecutionContext.Implicits.global
import scala.collection.mutable

import ChildJVMMain.{MessageEnd, MessageStart}
import Status.*

/** Vulpix spawns JVM subprocesses (`numberOfWorkers`) in order to run tests
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
trait RunnerOrchestration:

  /** The maximum amount of active runners, which contain a child JVM */
  def numberOfWorkers: Int

  /** The maximum duration the child process is allowed to consume before
   *  getting destroyed
   */
  def maxDuration: Duration

  /** Destroy and respawn process after each test */
  def safeMode: Boolean

  /** Open JDI connection for testing the debugger */
  def debugMode: Boolean = false

  /** Each method of Debuggee can be called only once, in the order of definition.*/
  trait Debuggee:
    /** read the jdi port to connect the debugger */
    def readJdiPort(): Int
    /** start the main method in the background */
    def launch(): Unit
    /** wait until the end of the main method */
    def exit(): Status

  /** Kill all processes */
  def cleanup() = monitor.killAll()

  private val monitor = new RunnerMonitor
  export monitor.{debugMain/*, runMain*/}
  def runMain(classPath: String, toolArgs: ToolArgs)(using SummaryReporting): Status =
    monitor.runMain(classPath, toolArgs) // scala-js overrides and requires toolArgs

  /** The runner monitor object keeps track of child JVM processes by keeping
   *  them in two structures - one for free, and one for busy children.
   *
   *  When a user calls `runMain` the monitor makes takes a free JVM and blocks
   *  until the run is complete - or `maxDuration` has passed. It then performs
   *  cleanup by returning the used JVM to the free list, or respawning it if
   *  it died
   */
  private class RunnerMonitor:

    /** Runs a `Test` class's main method from the specified `classpath`. */
    def runMain(classPath: String, toolArgs: ToolArgs)(using SummaryReporting): Status =
      withRunner(_.runMain(classPath))

    /** Provide a Debuggee for debugging the Test class's main method.
     *  @param f the debugging flow: set breakpoints, launch main class, pause, step, evaluate, exit etc
     */
    def debugMain(classPath: String)(f: Debuggee => Unit)(using SummaryReporting): Unit =
      withRunner(_.debugMain(classPath)(f))

    private class RunnerProcess(p: Process):
      private val stdout = BufferedReader(InputStreamReader(p.getInputStream(), UTF_8))
      private val stdin = PrintStream(p.getOutputStream(), /* autoFlush = */ true)

      def readLine(): String =
        stdout.readLine() match
        case s"Listening for transport dt_socket at address: $port" =>
          throw IOException(
            "Unexpected transport dt_socket message." +
            " The port is going to be lost and no debugger will be able to connect."
          )
        case line => line

      def printLine(line: String): Unit = stdin.println(line)

      def getJdiPort(): Int =
        stdout.readLine() match
        case s"Listening for transport dt_socket at address: $port" => port.toInt
        case line => throw IOException(s"Failed getting JDI port of child JVM: got $line")

      def isAlive: Boolean = p.isAlive // export p.isAlive sans parens

      export p.{exitValue, destroy}
    end RunnerProcess

    private class Runner(process: RunnerProcess):
      /** Checks whether the underlying process is still alive. */
      def isAlive: Boolean = process.isAlive

      /** Destroys the underlying process and kills IO streams. */
      def kill(): Unit = process.destroy()

      /** Blocks less than `maxDuration` while running `Test.main` from `dir`. */
      def runMain(classPath: String): Status = awaitStatus(startMain(classPath))

      def debugMain(classPath: String)(f: Debuggee => Unit): Status =
        val debuggee = new Debuggee:
          private var mainFuture: Future[Status] | Null = null
          def readJdiPort(): Int = process.getJdiPort()
          def launch(): Unit = mainFuture = startMain(classPath)
          def exit(): Status = awaitStatus(mainFuture.nn)

        try
          f(debuggee)
          debuggee.exit()
        catch case e: Throwable => Failure("Bad debug")
      end debugMain

      private def startMain(classPath: String): Future[Status] =
        // pass classpath to running process
        process.printLine(classPath)

        def readChildOutput =
          val sb = StringBuilder()
          var ok = false
          while
            val line = process.readLine()
            line != null && {
              ok = line == MessageStart
              !ok
            }
          do () // Discard all messages until the test starts

          if ok then
            ok = false
            var childOutput: String | Null = null
            while
              childOutput = process.readLine()
              childOutput != null && {
                ok = childOutput == MessageEnd
                !ok
              }
            do // Collect all messages until the test ends
              sb.append(childOutput).append(System.lineSeparator)

          if ok && isAlive then
            Success(sb.toString)
          else
            Failure(sb.toString)
        Future(readChildOutput)
      end startMain

      // wait status of the main class execution
      private def awaitStatus(future: Future[Status]): Status =
        try Await.result(future, maxDuration)
        catch case _: TimeoutException => Timeout
    end Runner

    /** Create a process which has the classpath of the `ChildJVMMain` and the
     *  scala library.
     */
    private def createProcess(): RunnerProcess =
      val url = classOf[ChildJVMMain.type].getProtectionDomain.getCodeSource.getLocation
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
      while freeRunners.isEmpty && busyRunners.size >= numberOfWorkers
      do wait()

      val runner =
        if freeRunners.isEmpty then Runner(createProcess())
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

    private def discardRunner(runner: Runner): Unit = synchronized {
      busyRunners -= runner
    }

    private def withRunner(op: Runner => Status)(using SummaryReporting): Status =
      val runner = getRunner()
      val status = op(runner)
      if safeMode || !status.isSuccess then
        discardRunner(runner)
      else
        freeRunner(runner)
      status

    def killAll(): Unit =
      freeRunners.foreach(_.kill())
      busyRunners.foreach(_.kill())

    // On shutdown, we need to kill all runners:
    sys.addShutdownHook(killAll())
  end RunnerMonitor
