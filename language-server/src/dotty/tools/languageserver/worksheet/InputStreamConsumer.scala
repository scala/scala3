package dotty.tools.languageserver.worksheet

import java.io.{InputStream, IOException}
import java.util.Scanner

class InputStreamConsumer(in: InputStream) {
  private val scanner =
    new Scanner(in).useDelimiter(InputStreamConsumer.delimiter)

  /** Finds and returns the next complete token from this input stream.
   *
   *  A complete token is preceded and followed by input that matches the delimiter pattern.
   *  This method may block while waiting for input
   */
  def next(): String =
    try scanner.next()
    catch {
      case _: NoSuchElementException =>
        throw new IOException("InputStream closed")
    }
}

object InputStreamConsumer {
  def delimiter = "##!!##"
}
