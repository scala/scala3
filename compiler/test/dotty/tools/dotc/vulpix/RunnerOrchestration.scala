package dotty
package tools
package dotc
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

trait RunnerOrchestration {

  /** The maximum amount of active runners, which contain a child JVM */
  val numberOfSlaves: Int

  /** The maximum duration the child process is allowed to consume before
   *  getting destroyed
   */
  val maxDuration: Duration

  /** Destroy and respawn process after each test */
  val safeMode: Boolean

  /** Running a `Test` class's main method from the specified `dir` */
  def runMain(dir: JFile): Status = monitor.runMain(dir)

  private[this] val monitor = new RunnerMonitor

  private class RunnerMonitor {

    def runMain(dir: JFile): Status = withRunner(_.runMain(dir))

    private class Runner(private var process: Process) {
      private[this] val ois = new ObjectInputStream(process.getInputStream)
      private[this] val oos = new ObjectOutputStream(process.getOutputStream)

      def kill(): Unit = {
        if (process ne null) process.destroy()
        process = null
      }

      def runMain(dir: JFile): Status = {
        assert(process ne null,
          "Runner was killed and then reused without setting a new process")

        // Makes the encapsulating RunnerMonitor spawn a new runner
        def respawn(): Unit = {
          process.destroy()
          process = createProcess
        }

        // pass file to running process
        oos.writeObject(dir)

        // Create a future reading the object:
        val readObject = Future(ois.readObject().asInstanceOf[Status])

        // Await result for `maxDuration` and then timout and destroy the
        // process:
        val status =
          try Await.result(readObject, maxDuration)
          catch { case _: TimeoutException => { Timeout } }

        // Handle failure of the VM:
        status match {
          case _ if safeMode => respawn()
          case status: Failure => respawn()
          case Timeout => respawn()
          case _ => ()
        }

        // return run status:
        status
      }
    }

    private def createProcess: Process = ???

    private[this] val allRunners =
      List.fill(numberOfSlaves)(new Runner(createProcess))

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
