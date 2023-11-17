package dotty.tools.languageserver.worksheet

import dotty.tools.dotc.core.Contexts.Context

import java.io.{File, PrintStream, IOException}
import java.lang.ProcessBuilder.Redirect
import java.util.concurrent.CancellationException
import java.util.{Timer, TimerTask}

import org.eclipse.lsp4j.jsonrpc.CancelChecker

private object Evaluator {

  private val javaExec: Option[String] = {
    val bin = new File(scala.util.Properties.javaHome, "bin")
    val java = new File(bin, if (scala.util.Properties.isWin) "java.exe" else "java")

    if (java.exists()) Some(java.getAbsolutePath())
    else None
  }

  /**
   * The most recent Evaluator that was used. It can be reused if the user classpath hasn't changed
   * between two calls.
   */
  private var previousEvaluator: Option[(String, Evaluator)] = None

  /**
   * Get a (possibly reused) Evaluator and set cancel checker.
   *
   * @param cancelChecker The token that indicates whether evaluation has been cancelled.
   * @return A JVM running the REPL.
   */
  def get(cancelChecker: CancelChecker)(implicit ctx: Context): Option[Evaluator] = synchronized {
    val classpath = ctx.settings.classpath.value
    previousEvaluator match {
      case Some(cp, evaluator) if evaluator.isAlive && cp == classpath =>
        evaluator.reset(cancelChecker)
        Some(evaluator)
      case _ =>
        previousEvaluator.foreach(_._2.exit())
        val newEvaluator = javaExec.map(new Evaluator(_, ctx.settings.classpath.value, cancelChecker))
        previousEvaluator = newEvaluator.map(jvm => (classpath, jvm))
        newEvaluator
    }
  }
}

/**
 * Represents a JVM running the REPL, ready for evaluation.
 *
 * @param javaExec      The path to the `java` executable.
 * @param userClasspath The REPL classpath
 * @param cancelChecker The token that indicates whether evaluation has been cancelled.
 */
private class Evaluator private (javaExec: String,
                                 userClasspath: String,
                                 private var cancelChecker: CancelChecker) {
  private val process =
    new ProcessBuilder(
      javaExec,
      "-classpath", scala.util.Properties.javaClassPath,
      ReplProcess.getClass.getName.stripSuffix("$"),
      "-classpath", userClasspath,
      "-color:never")
       .redirectErrorStream(true)
       .start()

  // The stream that we use to send commands to the REPL
  private val processInput = new PrintStream(process.getOutputStream())

  // Messages coming out of the REPL
  private val processOutput = new InputStreamConsumer(process.getInputStream())

  // A timer that monitors cancellation
  private val cancellationTimer = new Timer()
  locally {
    val task = new TimerTask {
      def run(): Unit =
        if (!isAlive)
          cancellationTimer.cancel()
        else
          try cancelChecker.checkCanceled()
          catch { case _: CancellationException => exit() }
    }
    val checkCancelledDelayMs = 50L
    cancellationTimer.schedule(task, checkCancelledDelayMs, checkCancelledDelayMs)
  }


  /** Is the process that runs the REPL still alive? */
  def isAlive: Boolean = process.isAlive

  /**
   * Submit `command` to the REPL, wait for the result.
   *
   * @param command The command to evaluate.
   * @return The result from the REPL.
   */
  def eval(command: String): String = {
    processInput.print(command)
    processInput.print(InputStreamConsumer.delimiter)
    processInput.flush()

    try processOutput.next().trim
    catch { case _: IOException => "" }
  }

  /**
   * Reset the REPL to its initial state, update the cancel checker.
   */
  def reset(cancelChecker: CancelChecker): Unit = {
    this.cancelChecker = cancelChecker
    eval(":reset")
  }

  /** Terminate this JVM. */
  def exit(): Unit = {
    process.destroyForcibly()
    Evaluator.previousEvaluator = None
    cancellationTimer.cancel()
  }
}
