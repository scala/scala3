package dotty.tools.languageserver

import dotty.tools.dotc.ast.tpd.{DefTree, Template, Tree, TypeDef}
import dotty.tools.dotc.core.Contexts.Context
import dotty.tools.dotc.interactive.SourceTree
import dotty.tools.dotc.util.Positions.Position
import dotty.tools.dotc.util.SourceFile

import dotty.tools.dotc.core.Flags.Synthetic

import dotty.tools.repl.{ReplDriver, State}

import org.eclipse.lsp4j.jsonrpc.CancelChecker

import java.io.{File, InputStream, InputStreamReader, PrintStream}
import java.util.concurrent.CancellationException

object Worksheet {

  private val javaExec: Option[String] = {
    val isWindows = sys.props("os.name").toLowerCase().indexOf("win") >= 0
    val bin = new File(sys.props("java.home"), "bin")
    val java = new File(bin, if (isWindows) "java.exe" else "java")

    if (java.exists()) Some(java.getAbsolutePath())
    else None
  }

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
      implicit ctx: Context): Unit = {

    val replMain = dotty.tools.repl.WorksheetMain.getClass.getName.init
    val classpath = sys.props("java.class.path")
    val options = Array(javaExec.get, "-classpath", classpath, replMain) ++ replOptions
    val replProcess = new ProcessBuilder(options: _*).redirectErrorStream(true).start()

    // The stream that we use to send commands to the REPL
    val replIn = new PrintStream(replProcess.getOutputStream())

    // Messages coming out of the REPL
    val replOut = new ReplReader(replProcess.getInputStream())
    replOut.start()

    // The thread that monitors cancellation
    val cancellationThread = new CancellationThread(replOut, replProcess, cancelChecker)
    cancellationThread.start()

    // Wait for the REPL to be ready
    replOut.next()

    tree.tree match {
      case td @ TypeDef(_, template: Template) =>
        val executed = collection.mutable.Set.empty[(Int, Int)]

        template.body.foreach {
          case statement: DefTree if statement.symbol.is(Synthetic) =>
            ()

          case statement if executed.add(bounds(statement.pos)) =>
            val line = execute(replIn, statement, tree.source)
            val result = replOut.next().trim
            if (result.nonEmpty) sendMessage(encode(result, line))

          case _ =>
            ()
        }
    }
  }

  /**
   * Extract `tree` from the source and evaluate it in the REPL.
   *
   * @param replIn     A stream to send commands to the REPL.
   * @param tree       The compiled tree to evaluate.
   * @param sourcefile The sourcefile of the worksheet.
   * @return The line in the sourcefile that corresponds to `tree`.
   */
  private def execute(replIn: PrintStream, tree: Tree, sourcefile: SourceFile): Int = {
    val source = sourcefile.content.slice(tree.pos.start, tree.pos.end).mkString
    val line = sourcefile.offsetToLine(tree.pos.end)
    replIn.println(source)
    replIn.flush()
    line
  }

  private def replOptions(implicit ctx: Context): Array[String] =
    Array("-color:never", "-classpath", ctx.settings.classpath.value)

  private def encode(message: String, line: Int): String =
    line + ":" + message

  private def bounds(pos: Position): (Int, Int) = (pos.start, pos.end)

  /**
   * Regularly check whether execution has been cancelled, kill REPL if it is.
   *
   * @param replReader    The ReplReader that reads the output of the REPL
   * @param process       The forked JVM that runs the REPL.
   * @param cancelChecker The token that reports cancellation.
   */
  private class CancellationThread(replReader: ReplReader, process: Process, cancelChecker: CancelChecker) extends Thread {
    private final val checkCancelledDelayMs = 50

    override def run(): Unit = {
      try {
        while (!Thread.interrupted()) {
          cancelChecker.checkCanceled()
          Thread.sleep(checkCancelledDelayMs)
        }
      } catch {
        case _: CancellationException =>
          replReader.interrupt()
          process.destroyForcibly()
      }
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
    }

    /** Block until the next message is ready. */
    def next(): String = synchronized {
      while (output.isEmpty) {
        wait()
      }

      val result = output.get
      notify()
      output = None
      result
    }
  }

}
