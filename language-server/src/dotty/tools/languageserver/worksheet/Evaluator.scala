package dotty.tools.languageserver.worksheet

import dotty.tools.dotc.core.Contexts.Context

import java.io.{File, PrintStream}

import org.eclipse.lsp4j.jsonrpc.CancelChecker

private object Evaluator {

  private val javaExec: Option[String] = {
    val isWindows = sys.props("os.name").toLowerCase().indexOf("win") >= 0
    val bin = new File(sys.props("java.home"), "bin")
    val java = new File(bin, if (isWindows) "java.exe" else "java")

    if (java.exists()) Some(java.getAbsolutePath())
    else None
  }

  /**
   * The most recent Evaluator that was used. It can be reused if the user classpath hasn't changed
   * between two calls.
   */
  private[this] var previousEvaluator: Option[(String, Evaluator)] = None

  /**
   * Get a (possibly reused) Evaluator and set cancel checker.
   *
   * @param cancelChecker The token that indicates whether evaluation has been cancelled.
   * @return A JVM running the REPL.
   */
  def get(cancelChecker: CancelChecker)(implicit ctx: Context): Option[Evaluator] = {
    val classpath = ctx.settings.classpath.value
    previousEvaluator match {
      case Some(cp, evaluator) if evaluator.isAlive() && cp == classpath =>
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
                                 cancelChecker: CancelChecker) {
  private val process =
    new ProcessBuilder(
      javaExec,
      "-classpath", sys.props("java.class.path"),
      dotty.tools.repl.WorksheetMain.getClass.getName.init,
      "-classpath", userClasspath,
      "-color:never")
       .redirectErrorStream(true)
       .start()

  // The stream that we use to send commands to the REPL
  private val processInput = new PrintStream(process.getOutputStream())

  // Messages coming out of the REPL
  private val processOutput = new ReplReader(process.getInputStream())
  processOutput.start()

  // The thread that monitors cancellation
  private val cancellationThread = new CancellationThread(cancelChecker, this)
  cancellationThread.start()

  // Wait for the REPL to be ready
  processOutput.next()

  /** Is the process that runs the REPL still alive? */
  def isAlive(): Boolean = process.isAlive()

  /**
   * Submit `command` to the REPL, wait for the result.
   *
   * @param command The command to evaluate.
   * @return The result from the REPL.
   */
  def eval(command: String): Option[String] = {
    processInput.println(command)
    processInput.flush()
    processOutput.next().map(_.trim)
  }

  /**
   * Reset the REPL to its initial state, update the cancel checker.
   */
  def reset(cancelChecker: CancelChecker): Unit = {
    cancellationThread.setCancelChecker(cancelChecker)
    eval(":reset")
  }

  /** Terminate this JVM. */
  def exit(): Unit = {
    processOutput.interrupt()
    process.destroyForcibly()
    Evaluator.previousEvaluator = None
    cancellationThread.interrupt()
  }

}

