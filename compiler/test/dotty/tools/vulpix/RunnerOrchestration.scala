package dotty.tools
package vulpix

import java.io.{
  File => JFile,
  InputStream, ObjectInputStream,
  OutputStream, ObjectOutputStream
}
import java.util.concurrent.TimeoutException

import scala.concurrent.duration.Duration
import scala.concurrent.{ Await, Future }
import scala.concurrent.ExecutionContext.Implicits.global
import scala.collection.mutable

import vulpix.Statuses._

trait RunnerOrchestration {

  /** The maximum amount of active runners, which contain a child JVM */
  def numberOfSlaves: Int

  /** The maximum duration the child process is allowed to consume before
   *  getting destroyed
   */
  def maxDuration: Duration

  /** Destroy and respawn process after each test */
  def safeMode: Boolean

  /** Running a `Test` class's main method from the specified `dir` */
  def runMain(dir: JFile): Status = monitor.runMain(dir)

  private[this] val monitor = new RunnerMonitor

  private class RunnerMonitor {

    def runMain(dir: JFile): Status = withRunner(_.runMain(dir))

    private class Runner(private var process: Process) {
      private[this] var ois: ObjectInputStream = _
      private[this] var oos: ObjectOutputStream = _

      /** Checks if `process` is still alive
       *
       *  When `process.exitValue()` is called on an active process the caught
       *  exception is thrown. As such we can know if the subprocess exited or
       *  not.
       *
       *  @note used for debug
       */
      def isAlive: Boolean =
        try { process.exitValue(); false }
        catch { case _: IllegalThreadStateException => true }

      /** Destroys the underlying process and kills IO streams */
      def kill(): Unit = {
        if (process ne null) process.destroy()
        process = null
        ois = null
        oos = null
      }

      /** Blocks less than `maxDuration` while running `Test.main` from `dir` */
      def runMain(dir: JFile): Status = {
        assert(process ne null,
          "Runner was killed and then reused without setting a new process")

        // Makes the encapsulating RunnerMonitor spawn a new runner
        def respawn(): Unit = {
          process.destroy()
          process = createProcess
          ois = null
          oos = null
        }

        if (oos eq null) oos = new ObjectOutputStream(process.getOutputStream)

        // pass file to running process
        oos.writeObject(dir)
        oos.flush()

        // Create a future reading the object:
        val readObject = Future {
          if (ois eq null) ois = new ObjectInputStream(process.getInputStream)
          ois.readObject().asInstanceOf[Status]
        }

        // Await result for `maxDuration` and then timout and destroy the
        // process:
        val status =
          try Await.result(readObject, maxDuration)
          catch { case _: TimeoutException =>  new Timeout() }

        // Handle failure of the VM:
        status match {
          case _ if safeMode => respawn()
          case _: Failure => respawn()
          case _: Timeout => respawn()
          case _ => ()
        }
        status
      }
    }

    private def createProcess: Process = {
      val sep = sys.props("file.separator")
      val cp = sys.props("java.class.path")
      val java = sys.props("java.home") + sep + "bin" + sep + "java"
      new ProcessBuilder(java, "-cp", cp, "dotty.tools.dotc.vulpix.ChildMain")//classOf[ChildMain].getName)
        .redirectErrorStream(true)
        .redirectInput(ProcessBuilder.Redirect.PIPE)
        .redirectOutput(ProcessBuilder.Redirect.PIPE)
        .start()
    }

    private[this] val allRunners = List.fill(numberOfSlaves)(new Runner(createProcess))

    private[this] val freeRunners = mutable.Queue(allRunners: _*)
    private[this] val busyRunners = mutable.Set.empty[Runner]

    private def getRunner(): Runner = synchronized {
      while (freeRunners.isEmpty) wait()

      val runner = freeRunners.dequeue()
      busyRunners += runner

      notify()
      runner
    }

    private def freeRunner(runner: Runner): Unit = synchronized {
      freeRunners.enqueue(runner)
      busyRunners -= runner
      notify()
    }

    private def withRunner[T](op: Runner => T): T = {
      val runner = getRunner()
      val result = op(runner)
      freeRunner(runner)
      result
    }

    private def killAll(): Unit = allRunners.foreach(_.kill())

    // On shutdown, we need to kill all runners:
    sys.addShutdownHook(killAll())
    // If for some reason the test runner (i.e. sbt) doesn't kill the VM, we
    // need to clean up ourselves.
    SummaryReport.addCleanup(killAll)
  }
}
