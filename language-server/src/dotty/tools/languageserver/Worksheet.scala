package dotty.tools.languageserver

import dotty.tools.dotc.ast.tpd.{DefTree, Template, Tree, TypeDef}
import dotty.tools.dotc.core.Contexts.Context
import dotty.tools.dotc.interactive.SourceTree
import dotty.tools.dotc.util.Positions.Position
import dotty.tools.dotc.util.SourceFile

import dotty.tools.dotc.core.Flags.Synthetic

import org.eclipse.lsp4j.jsonrpc.CancelChecker

import java.io.{File, InputStream, InputStreamReader, PrintStream}
import java.util.concurrent.CancellationException

object Worksheet {

  /**
   * Evaluate `tree` as a worksheet using the REPL.
   *
   * @param tree          The top level object wrapping the worksheet.
   * @param sendMessage   A mean of communicating the results of evaluation back.
   * @param cancelChecker A token to check whether execution should be cancelled.
   */
  def evaluate(tree: SourceTree,
               sendMessage: String => Unit,
               cancelChecker: CancelChecker)(
      implicit ctx: Context): Unit = synchronized {

    Evaluator.get(cancelChecker) match {
      case None =>
        sendMessage(encode("Couldn't start JVM.", 1))
      case Some(evaluator) =>
        tree.tree match {
          case td @ TypeDef(_, template: Template) =>
            val executed = collection.mutable.Set.empty[(Int, Int)]

            template.body.foreach {
              case statement: DefTree if statement.symbol.is(Synthetic) =>
                ()

              case statement if evaluator.isAlive() && executed.add(bounds(statement.pos)) =>
                try {
                  cancelChecker.checkCanceled()
                  val (line, result) = execute(evaluator, statement, tree.source)
                  if (result.nonEmpty) sendMessage(encode(result, line))
                } catch { case _: CancellationException => () }

              case _ =>
                ()
            }
        }
    }
  }

  /**
   * Extract `tree` from the source and evaluate it in the REPL.
   *
   * @param evaluator  The JVM that runs the REPL.
   * @param tree       The compiled tree to evaluate.
   * @param sourcefile The sourcefile of the worksheet.
   * @return The line in the sourcefile that corresponds to `tree`, and the result.
   */
  private def execute(evaluator: Evaluator, tree: Tree, sourcefile: SourceFile): (Int, String) = {
    val source = sourcefile.content.slice(tree.pos.start, tree.pos.end).mkString
    val line = sourcefile.offsetToLine(tree.pos.end)
    (line, evaluator.eval(source).getOrElse(""))
  }

  private def encode(message: String, line: Int): String =
    line + ":" + message

  private def bounds(pos: Position): (Int, Int) = (pos.start, pos.end)

}

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

/**
 * Regularly check whether execution has been cancelled, kill REPL if it is.
 */
private class CancellationThread(private[this] var cancelChecker: CancelChecker,
                                 evaluator: Evaluator) extends Thread {
  private final val checkCancelledDelayMs = 50

  override def run(): Unit = {
    try {
      while (evaluator.isAlive() && !Thread.interrupted()) {
        cancelChecker.checkCanceled()
        Thread.sleep(checkCancelledDelayMs)
      }
    } catch {
      case _: CancellationException => evaluator.exit()
      case _: InterruptedException => evaluator.exit()
    }
  }

  def setCancelChecker(cancelChecker: CancelChecker): Unit = {
    this.cancelChecker = cancelChecker
  }
}

/**
 * Reads the output from the REPL and makes it available via `next()`.
 *
 * @param stream The stream of messages coming out of the REPL.
 */
private class ReplReader(stream: InputStream) extends Thread {
  private val in = new InputStreamReader(stream)

  private[this] var output: Option[String] = None
  private[this] var closed: Boolean = false

  override def run(): Unit = synchronized {
    val prompt = "scala> "
    val buffer = new StringBuilder
    val chars = new Array[Char](256)
    var read = 0

    while (!Thread.interrupted() && { read = in.read(chars); read >= 0 }) {
      buffer.appendAll(chars, 0, read)
      if (buffer.endsWith(prompt)) {
        output = Some(buffer.toString.stripSuffix(prompt))
        buffer.clear()
        notify()
        wait()
      }
    }
    closed = true
    notify()
  }

  /** Block until the next message is ready. */
  def next(): Option[String] = synchronized {

    while (!closed && output.isEmpty) {
      wait()
    }

    val result = output
    notify()
    output = None
    result
  }
}
