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
import Process._
import BasicIO.{LazilyListed, Streamed, Uncloseable}
import Uncloseable.protect

import java.io.{FileInputStream, FileOutputStream}
import java.util.concurrent.LinkedBlockingQueue

import scala.util.control.NonFatal

private[process] trait ProcessBuilderImpl {
  self: ProcessBuilder.type =>

  private[process] final class DaemonBuilder(underlying: ProcessBuilder) extends AbstractBuilder {
    /** Starts the process represented by the underlying builder, with the threads that run
     *  the supplied `ProcessIO`'s input, output, and error handlers marked as daemon threads.
     *
     *  Threads a compound command starts outside those handlers, such as the transfer threads
     *  of a pipe, are not daemonized by this builder.
     *
     *  @param io the `ProcessIO` that handles the process's streams; a daemonized copy of it is used
     *  @return the started `Process`
     */
    override def run(io: ProcessIO): Process = underlying.run(io.daemonized())
  }

  private[process] final class Dummy(override val toString: String, exitValue: => Int) extends AbstractBuilder {
    /** Creates a `Process` that starts no external command and whose exit value is the one
     *  supplied to this builder.
     *
     *  @param io the `ProcessIO` that would handle the process's streams; it is ignored, since no external command runs
     *  @return a `Process` yielding this builder's exit value
     */
    override def run(io: ProcessIO): Process = new DummyProcess(exitValue)
    /** Returns `true`, since a dummy command can be the target of a pipe. */
    override def canPipeTo = true
  }

  private[process] final class URLInput(url: URL) extends ThreadBuilder(url.toString) {
    /** Returns `false`, since copying the contents of a URL yields no exit code worth propagating. */
    override def hasExitValue = false
    /** Opens the URL and hands the resulting stream to `io` as the process output.
     *
     *  @param io the `ProcessIO` whose `processOutput` function consumes the URL's contents
     */
    override def runImpl(io: ProcessIO): Unit = io.processOutput(protect(url.openStream()))
  }

  // Because the argument must be call-by-name to re-create the stream every time,
  // this class should not be reused in a context where the call-by-name argument does something sensitive,
  // like `url.openStream()`, since otherwise there is a hypothetical possibility of a Java deserialization gadget chain.
  private[process] final class IStreamBuilder(stream: => InputStream, label: String) extends ThreadBuilder(label) {
    /** Returns `false`, since copying the contents of an input stream yields no exit code worth propagating. */
    override def hasExitValue = false
    /** Evaluates the by-name stream and hands it to `io` as the process output, guarding the
     *  JVM's own standard input against being closed.
     *
     *  @param io the `ProcessIO` whose `processOutput` function consumes the stream
     */
    override def runImpl(io: ProcessIO): Unit = io.processOutput(protect(stream))
  }

  private[process] final class FileInput(file: File) extends ThreadBuilder(file.getAbsolutePath) {
    /** Returns `false`, since copying the contents of a file yields no exit code worth propagating. */
    override def hasExitValue = false
    /** Opens the file for reading and hands the resulting stream to `io` as the process output.
     *
     *  @param io the `ProcessIO` whose `processOutput` function consumes the file's contents
     */
    override def runImpl(io: ProcessIO): Unit = io.processOutput(protect(new FileInputStream(file)))
  }

  // Same remark as IStreamBuilder
  private[process] final class OStreamBuilder(stream: => OutputStream, label: String) extends ThreadBuilder(label) {
    /** Returns `false`, since writing to an output stream yields no exit code worth propagating. */
    override def hasExitValue = false
    /** Evaluates the by-name stream and hands it to `io` to be filled as the process input,
     *  guarding the JVM's own standard output and error against being closed.
     *
     *  @param io the `ProcessIO` whose `writeInput` function writes to the stream
     */
    override def runImpl(io: ProcessIO): Unit = io.writeInput(protect(stream))
  }

  private[process] final class FileOutput(file: File, append: Boolean) extends ThreadBuilder(file.getAbsolutePath) {
    /** Returns `false`, since writing to a file yields no exit code worth propagating. */
    override def hasExitValue = false
    /** Opens the file for writing, appending to or truncating it as this builder was
     *  configured, and hands the resulting stream to `io` to be filled as the process input.
     *
     *  @param io the `ProcessIO` whose `writeInput` function writes to the stream
     */
    override def runImpl(io: ProcessIO): Unit = io.writeInput(protect(new FileOutputStream(file, append)))
  }

  private[process] abstract class ThreadBuilder(
    override val toString: String
  ) extends AbstractBuilder {

    /** Performs this builder's I/O work, which `run` carries out on a separate thread.
     *
     *  @param io the `ProcessIO` supplying the functions that handle the streams
     */
    def runImpl(io: ProcessIO): Unit

    /** Spawns a thread that performs `runImpl` and returns immediately.
     *
     *  @param io the `ProcessIO` supplying the functions that handle the streams
     *  @return a `Process` backed by that thread, whose exit value is 0 if `runImpl` completed normally and 1 if it threw
     */
    override def run(io: ProcessIO): Process = {
      val success = new LinkedBlockingQueue[Boolean](1)
      def go(): Unit = {
        var ok = false
        try {
          runImpl(io)
          ok = true
        } finally success.put(ok)
      }
      val t = Spawn("ThreadProcess", io.daemonizeThreads)(go())
      new ThreadProcess(t, success)
    }
  }

  /** Represents a simple command without any redirection or combination.
   *
   *  @param p the underlying `java.lang.ProcessBuilder` used to start the external process
   */
  private[process] class Simple(p: JProcessBuilder) extends AbstractBuilder {
    /** Starts the external command and spawns the threads that pump its input, output, and
     *  error streams through `io`.
     *
     *  No input thread is spawned when `io` writes input with `BasicIO.connectToStdIn`, in
     *  which case the command inherits the standard input of the current process, nor when
     *  it writes input with `BasicIO.connectNoOp`.  No error thread is spawned when the
     *  underlying `java.lang.ProcessBuilder` merges error output into standard output.
     *
     *  @param io the `ProcessIO` supplying the functions that handle the streams
     *  @return the started `Process`
     */
    override def run(io: ProcessIO): Process = {
      import java.lang.ProcessBuilder.Redirect.{INHERIT => Inherit}
      import io.{daemonizeThreads, processError, processOutput, writeInput}

      val inherit = writeInput eq BasicIO.connectToStdIn
      if (inherit) p.redirectInput(Inherit)

      val process = p.start() // start the external process

      // spawn threads that process the input, output, and error streams using the functions defined in `io`
      val inThread: Thread | Null =
        if (inherit || (writeInput eq BasicIO.connectNoOp)) null
        else Spawn("Simple-input", daemon = true)(writeInput(process.getOutputStream))
      val outThread = Spawn("Simple-output", daemonizeThreads)(processOutput(process.getInputStream()))
      val errorThread =
        if (p.redirectErrorStream) Nil
        else List(Spawn("Simple-error", daemonizeThreads)(processError(process.getErrorStream())))

      new SimpleProcess(process, inThread, outThread :: errorThread)
    }
    /** Returns the string form of the underlying `java.lang.ProcessBuilder`'s command. */
    override def toString() = p.command.toString
    /** Returns `true`, since a simple command can be the target of a pipe. */
    override def canPipeTo = true
  }

  private[scala] abstract class AbstractBuilder extends ProcessBuilder with Sink with Source {
    /** Returns this builder itself, which serves as its own source. */
    protected def toSource: AbstractBuilder = this
    /** Returns this builder itself, which serves as its own sink. */
    protected def toSink: AbstractBuilder = this

    private val defaultStreamCapacity = 4096

    def #|(other: ProcessBuilder): ProcessBuilder  = {
      require(other.canPipeTo, "Piping to multiple processes is not supported.")
      new PipedBuilder(this, other, toError = false)
    }
    def #||(other: ProcessBuilder): ProcessBuilder = new OrBuilder(this, other)
    def #&&(other: ProcessBuilder): ProcessBuilder = new AndBuilder(this, other)
    def ###(other: ProcessBuilder): ProcessBuilder = new SequenceBuilder(this, other)

    /** Returns the started `Process` for this builder, run with its output and error sent to
     *  the console and with no input given to it.
     */
    def run(): Process                                          = run(connectInput = false)
    /** Starts the process represented by this builder, sending its output and error to the
     *  console.
     *
     *  @param connectInput whether the process reads from the standard input of the current process
     *  @return the started `Process`
     */
    def run(connectInput: Boolean): Process                     = run(BasicIO.standard(connectInput))
    /** Starts the process represented by this builder, sending its output and error to `log`
     *  and giving it no input.
     *
     *  @param log the `ProcessLogger` to receive standard output and error
     *  @return the started `Process`
     */
    def run(log: ProcessLogger): Process                        = run(log, connectInput = false)
    /** Starts the process represented by this builder, sending its output and error to `log`.
     *
     *  @param log the `ProcessLogger` to receive standard output and error
     *  @param connectInput whether the process reads from the standard input of the current process
     *  @return the started `Process`
     */
    def run(log: ProcessLogger, connectInput: Boolean): Process = run(BasicIO(connectInput, log))

    def !!                      = slurp(None, withIn = false)
    def !!(log: ProcessLogger)  = slurp(Some(log), withIn = false)
    def !!<                     = slurp(None, withIn = true)
    def !!<(log: ProcessLogger) = slurp(Some(log), withIn = true)

    /** Returns the standard output of the process represented by this builder as a `LazyList`
     *  of lines that blocks until each line becomes available, starting that process as a side
     *  effect.  Standard error is sent to the console, and a non-zero exit code raises an
     *  exception after the last line.
     */
    def lazyLines: LazyList[String]                       = lazyLines(withInput = false, nonZeroException = true, None, defaultStreamCapacity)
    /** Starts the process represented by this builder and returns its standard output as a
     *  `LazyList` of lines that blocks until each line becomes available.  A non-zero exit
     *  code raises an exception after the last line.
     *
     *  @param log the `ProcessLogger` to receive standard error output
     *  @return a `LazyList` of the process's standard output lines, whose evaluation raises an
     *          exception once every line has been yielded if the exit code is non-zero
     */
    def lazyLines(log: ProcessLogger): LazyList[String]   = lazyLines(withInput = false, nonZeroException = true, Some(log), defaultStreamCapacity)
    /** Returns the standard output of the process represented by this builder as a `LazyList`
     *  of lines that blocks until each line becomes available, starting that process as a side
     *  effect.  Standard error is sent to the console, and a non-zero exit code raises no
     *  exception.
     */
    def lazyLines_! : LazyList[String]                    = lazyLines(withInput = false, nonZeroException = false, None, defaultStreamCapacity)
    /** Returns the standard output of the process represented by this builder as a `LazyList`
     *  of lines that blocks until each line becomes available, starting that process as a side
     *  effect.  Standard error is sent to the given `ProcessLogger`, and a non-zero exit code
     *  raises no exception.
     */
    def lazyLines_!(log: ProcessLogger): LazyList[String] = lazyLines(withInput = false, nonZeroException = false, Some(log), defaultStreamCapacity)
    /** Starts the process represented by this builder and returns its standard output as a
     *  `LazyList` of lines that blocks until each line becomes available.  Standard error is
     *  sent to the console, and a non-zero exit code raises an exception after the last line.
     *
     *  @param capacity the maximum number of lines to buffer before blocking the producer; it must
     *                  be non-null and positive, since a null or non-positive value fails when the
     *                  underlying buffer is created
     *  @return a `LazyList` of the process's standard output lines, whose evaluation raises an
     *          exception once every line has been yielded if the exit code is non-zero
     */
    def lazyLines(capacity: Integer): LazyList[String]                       = lazyLines(withInput = false, nonZeroException = true, None, capacity)
    /** Starts the process represented by this builder and returns its standard output as a
     *  `LazyList` of lines that blocks until each line becomes available.  A non-zero exit
     *  code raises an exception after the last line.
     *
     *  @param log the `ProcessLogger` to receive standard error output
     *  @param capacity the maximum number of lines to buffer before blocking the producer; it must
     *                  be non-null and positive, since a null or non-positive value fails when the
     *                  underlying buffer is created
     *  @return a `LazyList` of the process's standard output lines, whose evaluation raises an
     *          exception once every line has been yielded if the exit code is non-zero
     */
    def lazyLines(log: ProcessLogger, capacity: Integer): LazyList[String]   = lazyLines(withInput = false, nonZeroException = true, Some(log), capacity)
    /** Returns the standard output of the process represented by this builder as a `LazyList`
     *  of lines that blocks until each line becomes available, buffering at most `capacity`
     *  lines ahead of the consumer and starting that process as a side effect.  The capacity must
     *  be non-null and positive, since a null or non-positive value fails when the underlying
     *  buffer is created.  Standard error is sent to the console, and a non-zero exit code raises
     *  no exception.
     */
    def lazyLines_!(capacity: Integer) : LazyList[String]                    = lazyLines(withInput = false, nonZeroException = false, None, capacity)
    /** Returns the standard output of the process represented by this builder as a `LazyList`
     *  of lines that blocks until each line becomes available, buffering at most `capacity`
     *  lines ahead of the consumer and starting that process as a side effect.  The capacity must
     *  be non-null and positive, since a null or non-positive value fails when the underlying
     *  buffer is created.  Standard error is sent to the given `ProcessLogger`, and a non-zero exit
     *  code raises no exception.
     */
    def lazyLines_!(log: ProcessLogger, capacity: Integer): LazyList[String] = lazyLines(withInput = false, nonZeroException = false, Some(log), capacity)

    @deprecated("internal", since = "2.13.4") def lineStream: Stream[String]                       = lineStream(withInput = false, nonZeroException = true, None, defaultStreamCapacity)
    @deprecated("internal", since = "2.13.4") def lineStream(log: ProcessLogger): Stream[String]   = lineStream(withInput = false, nonZeroException = true, Some(log), defaultStreamCapacity)
    @deprecated("internal", since = "2.13.4") def lineStream_! : Stream[String]                    = lineStream(withInput = false, nonZeroException = false, None, defaultStreamCapacity)
    @deprecated("internal", since = "2.13.4") def lineStream_!(log: ProcessLogger): Stream[String] = lineStream(withInput = false, nonZeroException = false, Some(log), defaultStreamCapacity)
    @deprecated("internal", since = "2.13.4") def lineStream(capacity: Integer): Stream[String]                       = lineStream(withInput = false, nonZeroException = true, None, capacity)
    @deprecated("internal", since = "2.13.4") def lineStream(log: ProcessLogger, capacity: Integer): Stream[String]   = lineStream(withInput = false, nonZeroException = true, Some(log), capacity)
    @deprecated("internal", since = "2.13.4") def lineStream_!(capacity: Integer) : Stream[String]                    = lineStream(withInput = false, nonZeroException = false, None, capacity)
    @deprecated("internal", since = "2.13.4") def lineStream_!(log: ProcessLogger, capacity: Integer): Stream[String] = lineStream(withInput = false, nonZeroException = false, Some(log), capacity)

    def !                      = run(connectInput = false).exitValue()
    def !(io: ProcessIO)       = run(io).exitValue()
    def !(log: ProcessLogger)  = runBuffered(log, connectInput = false)
    def !<                     = run(connectInput = true).exitValue()
    def !<(log: ProcessLogger) = runBuffered(log, connectInput = true)

    /** Constructs a new builder which runs this command with all input/output threads marked
     *  as daemon threads.  This allows the creation of a long running process while still
     *  allowing the JVM to exit normally.
     *
     *  Note: not in the public API because it's not fully baked, but I need the capability
     *  for fsc.
     *
     *  @return a new `ProcessBuilder` that runs this command with all I/O threads daemonized
     */
    def daemonized(): ProcessBuilder = new DaemonBuilder(this)

    private def slurp(log: Option[ProcessLogger], withIn: Boolean): String = {
      val buffer = new StringBuffer
      val code   = this ! BasicIO(withIn, buffer, log)

      if (code == 0) buffer.toString
      else scala.sys.error("Nonzero exit value: " + code)
    }

    private def lazyLines(
      withInput: Boolean,
      nonZeroException: Boolean,
      log: Option[ProcessLogger],
      capacity: Integer
    ): LazyList[String] = {
      val lazilyListed = LazilyListed[String](nonZeroException, capacity)
      val process      = run(BasicIO(withInput, lazilyListed.process, log))

      // extract done from lazilyListed so that the anonymous function below closes over just the done and not the whole lazilyListed (see https://github.com/scala/bug/issues/12185)
      val done = lazilyListed.done

      Spawn("LazyLines") {
        done {
          try process.exitValue()
          catch {
            case NonFatal(_) => -2
          }
        }
      }
      lazilyListed.lazyList
    }

    @deprecated("internal", since = "2.13.4")
    private def lineStream(
      withInput: Boolean,
      nonZeroException: Boolean,
      log: Option[ProcessLogger],
      capacity: Integer
    ): Stream[String] = {
      val streamed = Streamed[String](nonZeroException, capacity)
      val process  = run(BasicIO(withInput, streamed.process, log))

      Spawn("LineStream")(streamed done process.exitValue())
      streamed.stream()
    }

    private def runBuffered(log: ProcessLogger, connectInput: Boolean) =
      log buffer run(log, connectInput).exitValue()

    /** Returns `false`, since a command cannot be the target of a pipe unless it says otherwise. */
    def canPipeTo = false
    /** Returns `true`, since a command's exit code is propagated to the user unless it says otherwise. */
    def hasExitValue = true
  }

  private[process] class URLImpl(url: URL) extends URLBuilder with Source {
    /** Returns a builder that opens this `URL` and produces its contents as process output. */
    protected def toSource: URLInput = new URLInput(url)
  }
  private[process] class FileImpl(base: File) extends FileBuilder with Sink with Source {
    /** Returns a builder that reads this file and produces its contents as process output. */
    protected def toSource: FileInput = new FileInput(base)
    /** Returns a builder that writes its process input to this file, replacing any existing contents. */
    protected def toSink: FileOutput = new FileOutput(base, append = false)

    def #<<(f: File): ProcessBuilder           = #<<(new FileInput(f))
    def #<<(u: URL): ProcessBuilder            = #<<(new URLInput(u))
    def #<<(s: => InputStream): ProcessBuilder = #<<(new IStreamBuilder(s, "<input stream>"))
    def #<<(b: ProcessBuilder): ProcessBuilder = new PipedBuilder(b, new FileOutput(base, append = true), toError = false)
  }

  private[process] abstract class BasicBuilder extends AbstractBuilder {
    /** Checks that `a` is not this builder, throwing an `IllegalArgumentException` if it is,
     *  since a compound command cannot contain itself.
     *
     *  @param a the component command to check
     */
    protected def checkNotThis(a: ProcessBuilder) = require(a != this, "Compound process '" + a + "' cannot contain itself.")
    /** Creates the process for this compound command and starts it.
     *
     *  @param io the `ProcessIO` supplying the functions that handle the streams
     *  @return the started `Process`
     */
    final def run(io: ProcessIO): Process = {
      val p = createProcess(io)
      p.start()
      p
    }
    /** Creates the process that carries out this compound command, without starting it.
     *
     *  @param io the `ProcessIO` supplying the functions that handle the streams
     *  @return an unstarted `BasicProcess` for this command
     */
    protected def createProcess(io: ProcessIO): BasicProcess
  }

  private[process] abstract class SequentialBuilder(
    a: ProcessBuilder,
    b: ProcessBuilder,
    operatorString: String
  ) extends BasicBuilder {

    checkNotThis(a)
    checkNotThis(b)
    /** Returns the two component commands joined by this builder's operator and enclosed in parentheses. */
    override def toString() = " ( " + a + " " + operatorString + " " + b + " ) "
  }

  private[process] class PipedBuilder(
    first: ProcessBuilder,
    second: ProcessBuilder,
    toError: Boolean
  ) extends SequentialBuilder(first, second, if (toError) "#|!" else "#|") {

    /** Creates a process that runs both commands concurrently, feeding the output of the
     *  first, or its error output when this builder pipes to error, into the input of the second.
     *
     *  @param io the `ProcessIO` supplying the functions that handle the streams left unpiped
     *  @return an unstarted `PipedProcesses` for the two commands
     */
    override def createProcess(io: ProcessIO): PipedProcesses = new PipedProcesses(first, second, io, toError)
  }

  private[process] class AndBuilder(
    first: ProcessBuilder,
    second: ProcessBuilder
  ) extends SequentialBuilder(first, second, "#&&") {
    /** Creates a process that runs the first command and then the second only if the first
     *  exits with zero.
     *
     *  @param io the `ProcessIO` supplying the functions that handle both commands' streams
     *  @return an unstarted `AndProcess` for the two commands
     */
    override def createProcess(io: ProcessIO): AndProcess = new AndProcess(first, second, io)
  }

  private[process] class OrBuilder(
    first: ProcessBuilder,
    second: ProcessBuilder
  ) extends SequentialBuilder(first, second, "#||") {
    /** Creates a process that runs the first command and then the second only if the first
     *  exits with a non-zero value.
     *
     *  @param io the `ProcessIO` supplying the functions that handle both commands' streams
     *  @return an unstarted `OrProcess` for the two commands
     */
    override def createProcess(io: ProcessIO): OrProcess = new OrProcess(first, second, io)
  }

  private[process] class SequenceBuilder(
    first: ProcessBuilder,
    second: ProcessBuilder
  ) extends SequentialBuilder(first, second, "###") {
    /** Creates a process that runs the first command and then the second, whatever the first
     *  command's exit value.
     *
     *  @param io the `ProcessIO` supplying the functions that handle both commands' streams
     *  @return an unstarted `ProcessSequence` for the two commands
     */
    override def createProcess(io: ProcessIO): ProcessSequence = new ProcessSequence(first, second, io)
  }
}
