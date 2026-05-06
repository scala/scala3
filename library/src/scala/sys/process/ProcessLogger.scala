/*
 * Scala (https://www.scala-lang.org)
 *
 * Copyright EPFL and Lightbend, Inc. dba Akka
 *
 * Licensed under Apache License 2.0
 * (http://www.apache.org/licenses/LICENSE-2.0).
 *
 * See the NOTICE file distributed with this work for
 * additional information regarding copyright ownership.
 */

package scala
package sys
package process

import scala.language.`2.13`
import java.io._

/** Encapsulates the output and error streams of a running process. This is used
 *  by [[scala.sys.process.ProcessBuilder]] when starting a process, as an
 *  alternative to [[scala.sys.process.ProcessIO]], which can be more difficult
 *  to use. Note that a `ProcessLogger` will be used to create a `ProcessIO`
 *  anyway. The object `BasicIO` has some functions to do that.
 *
 *  Here is an example that counts the number of lines in the normal and error
 *  output of a process:
 *  ```
 *  import scala.sys.process._
 *
 *  var normalLines = 0
 *  var errorLines = 0
 *  val countLogger = ProcessLogger(line => normalLines += 1,
 *                                 line => errorLines += 1)
 *  "find /etc" ! countLogger
 *  ```
 *
 *  @see [[scala.sys.process.ProcessBuilder]]
 */
trait ProcessLogger {
  /** Will be called with each line read from the process output stream.
   *
   *  @param s a lazily-evaluated line from the process standard output
   */
  def out(s: => String): Unit

  /** Will be called with each line read from the process error stream.
   *
   *  @param s a lazily-evaluated line from the process standard error
   */
  def err(s: => String): Unit

  /** If a process is begun with one of these `ProcessBuilder` methods:
   *  ```
   *    def !(log: ProcessLogger): Int
   *    def !<(log: ProcessLogger): Int
   *  ```
   *  The run will be wrapped in a call to buffer.  This gives the logger
   *  an opportunity to set up and tear down buffering.  At present the
   *  library implementations of `ProcessLogger` simply execute the body
   *  unbuffered.
   *
   *  @tparam T the return type of the buffered operation
   *  @param f the code to execute with buffering, evaluated by name
   */
  def buffer[T](f: => T): T
}

/** A [[scala.sys.process.ProcessLogger]] that writes output to a file.
 *
 *  @param file the file to which both standard and error output will be appended
 */
class FileProcessLogger(file: File) extends ProcessLogger with Closeable with Flushable {
  private val writer = (
    new PrintWriter(
      new BufferedWriter(
        new OutputStreamWriter(
          new FileOutputStream(file, true)
        )
      )
    )
  )
  def out(s: => String): Unit = writer.println(s)
  def err(s: => String): Unit = writer.println(s)
  def buffer[T](f: => T): T = f
  def close(): Unit = writer.close()
  def flush(): Unit = writer.flush()
}

/** Provides factories to create [[scala.sys.process.ProcessLogger]], which
 *  are used to capture output of [[scala.sys.process.ProcessBuilder]] commands
 *  when run.
 */
object ProcessLogger {
  /** Creates a [[scala.sys.process.ProcessLogger]] that redirects output to a `java.io.File`.
   *
   *  @param file the `java.io.File` to which output will be appended
   *  @return a `FileProcessLogger` that writes to the given file
   */
  def apply(file: File): FileProcessLogger = new FileProcessLogger(file)

  /** Creates a [[scala.sys.process.ProcessLogger]] that sends all output, standard and error,
   *  to the passed function.
   *
   *  @param fn the function to apply to each line of standard and error output
   *  @return a `ProcessLogger` that passes all output to `fn`
   */
  def apply(fn: String => Unit): ProcessLogger = apply(fn, fn)

  /** Creates a [[scala.sys.process.ProcessLogger]] that sends all output to the corresponding
   *  function.
   *
   *  @param fout the function that will receive each line of standard output
   *  @param ferr the function that will receive each line of standard error
   *  @return a `ProcessLogger` that passes output to `fout` and errors to `ferr`
   */
  def apply(fout: String => Unit, ferr: String => Unit): ProcessLogger =
    new ProcessLogger {
      def out(s: => String): Unit = fout(s)
      def err(s: => String): Unit = ferr(s)
      def buffer[T](f: => T): T = f
    }
}
