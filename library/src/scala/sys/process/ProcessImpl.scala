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

package scala.sys.process

import scala.language.`2.13`
import processInternal.*
import java.util.concurrent.LinkedBlockingQueue
import java.io.{PipedInputStream, PipedOutputStream}
import scala.annotation.{nowarn, tailrec}

private[process] trait ProcessImpl {
  self: Process.type =>

  /** Runs provided code in a new Thread and returns the Thread instance. */
  private[process] object Spawn {
    /** Runs `f` in a newly created thread, which is started before returning.
     *
     *  @param prefix a label prepended to the thread's name, for diagnostics
     *  @param daemon whether the new thread is a daemon thread, and so does not keep the JVM alive
     *  @param f the by-name computation to run in the new thread
     *  @return the started thread running `f`
     */
    def apply(prefix: String, daemon: Boolean = false)(f: => Unit): Thread = {
      val thread = new Thread() { override def run() = f }
      thread.setName(prefix + "-spawn-" + thread.getName)
      thread.setDaemon(daemon)
      thread.start()
      thread
    }
  }
  private[process] object Future {
    /** Evaluates `f` in a new thread, started before returning.
     *
     *  @tparam T the type of the value computed by `f`
     *  @param f the by-name computation to evaluate in the new thread
     *  @return a pair of the thread evaluating `f` and a function that blocks until the
     *          result is available, rethrowing any exception thrown by `f`
     */
    def apply[T](f: => T): (Thread, () => T) = {
      val result = new LinkedBlockingQueue[Either[Throwable, T]](1)
      def run(): Unit = {
        val value = try Right(f) catch { case e: Exception => Left(e) }
        result.put(value)
      }

      val t = Spawn("Future")(run())

      (t, () => result.take() match {
        case Right(value)    => value
        case Left(exception) => throw exception
      })
    }
  }

  private[process] class AndProcess(
    a: ProcessBuilder,
    b: ProcessBuilder,
    io: ProcessIO
  ) extends SequentialProcess(a, b, io, _ == 0)

  private[process] class OrProcess(
    a: ProcessBuilder,
    b: ProcessBuilder,
    io: ProcessIO
  ) extends SequentialProcess(a, b, io, _ != 0)

  private[process] class ProcessSequence(
    a: ProcessBuilder,
    b: ProcessBuilder,
    io: ProcessIO
  ) extends SequentialProcess(a, b, io, _ => true)

  private[process] class SequentialProcess(
    a: ProcessBuilder,
    b: ProcessBuilder,
    io: ProcessIO,
    evaluateSecondProcess: Int => Boolean
  ) extends CompoundProcess {

    /** Runs `a` and then, if `evaluateSecondProcess` accepts its exit code, runs `b`, yielding the exit code of
     *  the last process run in `Some`, or `None` if waiting for either process is interrupted.
     */
    protected override def runAndExitValue() = {
      val first = a.run(io)
      runInterruptible(first.exitValue())(first.destroy()) flatMap { codeA =>
        if (evaluateSecondProcess(codeA)) {
          val second = b.run(io)
          runInterruptible(second.exitValue())(second.destroy())
        }
        else Some(codeA)
      }
    }
  }

  private[process] abstract class BasicProcess extends Process {
    /** Starts this process running, without waiting for it to exit. */
    def start(): Unit
  }

  private[process] abstract class CompoundProcess extends BasicProcess {
    /** Returns `true` if the thread running the underlying processes has not yet terminated. */
    def isAlive()   = processThread.isAlive()
    /** Destroys this process by interrupting the thread running the underlying processes. */
    def destroy()   = destroyer()
    /** Blocks until this process exits and returns its exit code, throwing a `RuntimeException` if no exit code was
     *  produced, as happens when the process is destroyed.
     */
    def exitValue() = futureValue() getOrElse scala.sys.error("No exit code: process destroyed.")
    /** Starts this process by forcing the threads that run the underlying processes to be created. */
    def start()     = { futureThread ;() }

    protected lazy val (processThread, (futureThread, futureValue: (() => Option[Int])), destroyer) = {
      val code = new LinkedBlockingQueue[Option[Int]](1)
      val thread = Spawn("CompoundProcess") {
        var value: Option[Int] = None
        try value = runAndExitValue()
        catch {
          case _: IndexOutOfBoundsException
             | _: IOException
             | _: NullPointerException
             | _: SecurityException
             | _: UnsupportedOperationException
          => value = Some(-1)
        }
        finally code.put(value)
      }

      (
        thread,
        Future(code.take()),          // thread.join()
        () => thread.interrupt()
      )
    }

    /** Starts and blocks until the exit value is available and then returns it in Some.  Returns None if destroyed (use 'run'). */
    protected def runAndExitValue(): Option[Int]

    /** Evaluates `action`, performing `destroyImpl` instead if evaluating `action` throws an `InterruptedException`.
     *
     *  @tparam T the type of the value computed by `action`
     *  @param action the by-name computation to evaluate
     *  @param destroyImpl the clean-up to perform if `action` throws an `InterruptedException`
     *  @return the result of `action` in `Some`, or `None` if `action` threw an `InterruptedException`
     */
    protected def runInterruptible[T](action: => T)(destroyImpl: => Unit): Option[T] = {
      try   Some(action)
      catch onInterrupt { destroyImpl; None }
    }
  }

  private[process] class PipedProcesses(a: ProcessBuilder, b: ProcessBuilder, defaultIO: ProcessIO, toError: Boolean) extends CompoundProcess {
    /** Creates the thread that reads the output of the first process into the pipe, labeled with that process. */
    protected def newSource: PipeSource = new PipeSource(a.toString)
    /** Creates the thread that writes the contents of the pipe to the input of the second process, labeled with that process. */
    protected def newSink:   PipeSink   = new PipeSink(b.toString)
    /** Runs both processes connected by a freshly created pipe source and sink. */
    protected override def runAndExitValue() = runAndExitValue(newSource, newSink)
    /** Runs both processes with the output of the first connected through `source` and `sink` to the input of the second.
     *
     *  The first process writes to its error stream rather than its output stream when this
     *  is a pipe to error. All of the pipe threads and processes are released if either
     *  process fails to start or the pipeline is interrupted.
     *
     *  @param source the pipe thread reading the output of the first process
     *  @param sink the pipe thread writing to the input of the second process
     *  @return the exit code of the second process in `Some`, or that of the first process
     *          if the second builder has no exit value of its own; `None` if interrupted
     */
    protected def runAndExitValue(source: PipeSource, sink: PipeSink): Option[Int] = {
      source connectOut sink
      source.start()
      sink.start()

      /* Release PipeSource, PipeSink and Process in the correct order.
       * If once connect Process with Source or Sink, then the order of releasing them
       * must be Source -> Sink -> Process, otherwise IOException will be thrown.
       */
      def releaseResources(so: PipeSource, sk: PipeSink, ps: Process*) = {
        so.release()
        sk.release()
        ps.foreach(_.destroy())
      }

      val firstIO =
        if (toError) defaultIO.withError(source.connectIn)
        else defaultIO.withOutput(source.connectIn)
      val secondIO = defaultIO.withInput(sink.connectOut)

      val second =
        try b.run(secondIO)
        catch onError { err =>
          releaseResources(source, sink)
          throw err
        }
      val first =
        try a.run(firstIO)
        catch onError { err =>
          releaseResources(source, sink, second)
          throw err
        }
      runInterruptible {
        val exit1 = first.exitValue()
        source.done()
        source.join()
        val exit2 = second.exitValue()
        sink.done()
        // Since file redirection (e.g. #>) is implemented as a piped process,
        // we ignore its exit value so cmd #> file doesn't always return 0.
        if (b.hasExitValue) exit2 else exit1
      } {
        releaseResources(source, sink, first, second)
      }
    }
  }

  private[process] abstract class PipeThread(isSink: Boolean, labelFn: () => String) extends Thread {
    /** Transfers data between the streams this thread connects, for as long as it has work to do. */
    def run(): Unit

    private[process] def runloop(src: InputStream, dst: OutputStream): Unit = {
      try     BasicIO.transferFully(src, dst)
      catch   ioFailure(ioHandler)
      finally BasicIO close {
        if (isSink) dst else src
      }
    }
    private def ioHandler(e: IOException): Unit = e.printStackTrace()
  }

  @nowarn("msg=Calling the external method .*Name") // setName+getName are safe to call in a constructor
  private[process] class PipeSource(label: => String) extends PipeThread(isSink = false, () => label) {
    setName(s"PipeSource($label)-$getName")
    protected val pipe = new PipedOutputStream
    protected val source = new LinkedBlockingQueue[Option[InputStream]](1)
    /** Copies each connected input stream into the pipe in turn, finishing once the stream being copied is exhausted
     *  and the end marker queued by `done()` is taken, or as soon as an `InterruptedException` is thrown, and closes
     *  the pipe on the way out.
     */
    override final def run(): Unit = {
      @tailrec def go(): Unit =
        source.take() match {
          case Some(in) => runloop(in, pipe) ; go()
          case None =>
        }
      try go()
      catch onInterrupt(())
      finally BasicIO close pipe
    }
    /** Hands `in` to this thread as the next stream to copy into the pipe, blocking while a previous stream is still queued.
     *
     *  @param in the stream, typically the output of a process, whose contents are copied into the pipe
     */
    def connectIn(in: InputStream): Unit = source.put(Some(in))
    /** Connects the far end of this source's pipe to `sink`, so that what is copied here is read by `sink`.
     *
     *  @param sink the pipe sink that consumes the contents of this source's pipe
     */
    def connectOut(sink: PipeSink): Unit = sink connectIn pipe
    /** Interrupts this thread, signals that no more input streams follow, and blocks until it has terminated. */
    def release(): Unit = {
      interrupt()
      done()
      join()
    }
    /** Signals that no further input streams will be connected, letting this thread finish. */
    def done() = source.put(None)
  }
  @nowarn("msg=Calling the external method .*Name") // setName+getName are safe to call in a constructor
  private[process] class PipeSink(label: => String) extends PipeThread(isSink = true, () => label) {
    setName(s"PipeSink($label)-$getName")
    protected val pipe = new PipedInputStream
    protected val sink = new LinkedBlockingQueue[Option[OutputStream]](1)
    /** Copies the pipe into each connected output stream in turn, finishing once the transfer in progress completes
     *  and the end marker queued by `done()` is taken, or as soon as an `InterruptedException` is thrown, and closes
     *  the pipe on the way out.
     */
    override def run(): Unit = {
      @tailrec def go(): Unit =
        sink.take() match {
          case Some(out) => runloop(pipe, out) ; go()
          case None =>
        }
      try go()
      catch onInterrupt(())
      finally BasicIO close pipe
    }
    /** Hands `out` to this thread as the next stream to receive the contents of the pipe, blocking while a previous stream is still queued.
     *
     *  @param out the stream, typically the input of a process, that the pipe's contents are copied to
     */
    def connectOut(out: OutputStream): Unit = sink.put(Some(out))
    /** Connects this sink's pipe to `pipeOut`, so that what is written there is read here.
     *
     *  @param pipeOut the piped output stream of the pipe source feeding this sink
     */
    def connectIn(pipeOut: PipedOutputStream): Unit = pipe.connect(pipeOut)
    /** Interrupts this thread, signals that no more output streams follow, and blocks until it has terminated. */
    def release(): Unit = {
      interrupt()
      done()
      join()
    }
    /** Signals that no further output streams will be connected, letting this thread finish. */
    def done() = sink.put(None)
  }

  /** A process that runs no native process, and is instead backed by a thread evaluating `action`.
   *  The implementation of `exitValue` waits until that thread produces a result before returning.
   *
   *  @param action the by-name computation whose result will be used as the exit value
   */
  private[process] class DummyProcess(action: => Int) extends Process {
    private val (thread, value) = Future(action)
    /** Returns `true` if the thread computing the exit value has not yet terminated. */
    override def isAlive() = thread.isAlive()
    /** Blocks until the computation finishes and returns its result as the exit code, rethrowing any exception the
     *  computation threw.
     */
    override def exitValue() = value()
    /** Does nothing, as there is no native process to destroy. */
    override def destroy(): Unit = { }
  }

  /** A thin wrapper around a java.lang.Process.
   *
   *  `outputThreads` are the Threads created to read from the
   *  output and error streams of the process.
   *
   *  `inputThread` is the Thread created to write to the input stream of
   *  the process. It may be null if stdin was inherited.
   *
   *  The implementation of `exitValue` interrupts `inputThread`
   *  and then waits until all I/O threads die before returning.
   *
   *  @param p the underlying `java.lang.Process` being wrapped
   *  @param inputThread the thread writing to the process's stdin, or null if stdin was inherited
   *  @param outputThreads the threads reading from the process's stdout and stderr streams
   */
  private[process] class SimpleProcess(p: JProcess, inputThread: Thread | Null, outputThreads: List[Thread]) extends Process {
    /** Returns `true` if the underlying `java.lang.Process` has not yet terminated. */
    override def isAlive() = p.isAlive()
    /** Blocks until the underlying process terminates, then waits for all of its output to be read and returns its
     *  exit code. If the wait for the process is interrupted, the input thread is still interrupted but the
     *  `InterruptedException` propagates without waiting for the output threads.
     */
    override def exitValue() = {
      try p.waitFor()                   // wait for the process to terminate
      finally interrupt()
      outputThreads foreach (_.join())  // this ensures that all output is complete before returning (waitFor does not ensure this)

      p.exitValue()
    }
    /** Destroys the underlying process, abandoning any of its output that has not yet been read. */
    override def destroy() = {
      try {
        outputThreads foreach (_.interrupt()) // on destroy, don't bother consuming any more output
        p.destroy()
      }
      finally interrupt()
    }
    // we interrupt the input thread to notify it that it can terminate
    private def interrupt(): Unit = if (inputThread != null) inputThread.interrupt()
  }
  private[process] final class ThreadProcess(thread: Thread, success: LinkedBlockingQueue[Boolean]) extends Process {
    /** Returns `true` if the thread doing this process's work has not yet terminated. */
    override def isAlive()   = thread.isAlive()
    /** Blocks until the thread reports its outcome and returns `0` if it succeeded, `1` otherwise, throwing an
     *  `InterruptedException` if the wait is interrupted.
     */
    override def exitValue() = if (success.take()) 0 else 1   // thread.join()
    /** Destroys this process by interrupting the thread doing its work. */
    override def destroy()   = thread.interrupt()
  }
}
