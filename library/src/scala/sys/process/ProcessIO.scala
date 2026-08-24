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
import processInternal._

/** This class is used to control the I/O of every
 *  [[scala.sys.process.Process]]. The functions used to create it will be
 *  called with the process streams once it has been started. It might not be
 *  necessary to use `ProcessIO` directly --
 *  [[scala.sys.process.ProcessBuilder]] can return the process output to the
 *  caller, or use a [[scala.sys.process.ProcessLogger]] which avoids direct
 *  interaction with a stream. One can even use the factories at `BasicIO` to
 *  create a `ProcessIO`, or use its helper methods when creating one's own
 *  `ProcessIO`.
 *
 *  When creating a `ProcessIO`, it is important to *close all streams* when
 *  finished, since the JVM might use system resources to capture the process
 *  input and output, and will not release them unless the streams are
 *  explicitly closed.
 *
 *  `ProcessBuilder` will call `writeInput`, `processOutput` and `processError`
 *  in separate threads, and if daemonizeThreads is true, they will all be
 *  marked as daemon threads.
 *
 *  @param writeInput Function that will be called with the `OutputStream` to
 *                   which all input to the process must be written. This will
 *                   be called in a newly spawned thread.
 *  @param processOutput Function that will be called with the `InputStream`
 *                      from which all normal output of the process must be
 *                      read from. This will be called in a newly spawned
 *                      thread.
 *  @param processError Function that will be called with the `InputStream` from
 *                     which all error output of the process must be read from.
 *                     This will be called in a newly spawned thread.
 *  @param daemonizeThreads Indicates whether the newly spawned threads that
 *                         will run `processOutput`, `processError` and
 *                         `writeInput` should be marked as daemon threads.
 *  @note Failure to close the passed streams may result in resource leakage.
 */
final class ProcessIO(
  /** The function called with the stream to which all input to the process must be written. */
  val writeInput: OutputStream => Unit,
  /** The function called with the stream from which all normal output of the process must be read. */
  val processOutput: InputStream => Unit,
  /** The function called with the stream from which all error output of the process must be read. */
  val processError: InputStream => Unit,
  /** Whether the threads spawned to run `processOutput` and `processError` are marked as daemon
   *  threads. The thread that runs `writeInput` is always a daemon, whatever this is set to.
   */
  val daemonizeThreads: Boolean
) {
  /** Creates a `ProcessIO` with `daemonizeThreads` set to `false`.
   *
   *  @param in function that will be called with the `OutputStream` to which all input to the process must be written
   *  @param out function that will be called with the `InputStream` from which all normal output of the process must be read
   *  @param err function that will be called with the `InputStream` from which all error output of the process must be read
   */
  def this(in: OutputStream => Unit, out: InputStream => Unit, err: InputStream => Unit) = this(in, out, err, daemonizeThreads = false)

  /** Creates a new `ProcessIO` with a different handler for the process input.
   *
   *  @param write the new function to handle the process input `OutputStream`
   *  @return a new `ProcessIO` with the specified input handler
   */
  def withInput(write: OutputStream => Unit): ProcessIO   = new ProcessIO(write, processOutput, processError, daemonizeThreads)

  /** Creates a new `ProcessIO` with a different handler for the normal output.
   *
   *  @param process the new function to handle the process standard output `InputStream`
   *  @return a new `ProcessIO` with the specified output handler
   */
  def withOutput(process: InputStream => Unit): ProcessIO = new ProcessIO(writeInput, process, processError, daemonizeThreads)

  /** Creates a new `ProcessIO` with a different handler for the error output.
   *
   *  @param process the new function to handle the process error output `InputStream`
   *  @return a new `ProcessIO` with the specified error handler
   */
  def withError(process: InputStream => Unit): ProcessIO  = new ProcessIO(writeInput, processOutput, process, daemonizeThreads)

  /** Creates a new `ProcessIO`, with `daemonizeThreads` true.
   *
   *  @return a copy of this `ProcessIO` with `daemonizeThreads` set to `true`
   */
  def daemonized(): ProcessIO = new ProcessIO(writeInput, processOutput, processError, daemonizeThreads = true)
}
