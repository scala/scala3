package dotty.tools.languageserver.worksheet

import java.io.{InputStream, InputStreamReader}

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
