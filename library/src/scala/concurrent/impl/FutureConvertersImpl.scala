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

package scala.concurrent.impl

import scala.language.`2.13`
import java.util.concurrent.{CompletableFuture, CompletionStage, TimeUnit}
import java.util.function.{BiConsumer, BiFunction, Consumer, Function => JFunction}

import scala.concurrent.Future
import scala.concurrent.impl.Promise.DefaultPromise
import scala.util.{Failure, Success, Try}

private[scala] object FutureConvertersImpl {
  /** A bridge from a Scala `Future` to a Java `CompletableFuture`. The non-async
   *  `CompletionStage` methods are redirected to their asynchronous variants, so that
   *  callbacks cannot hold the Scala `Future`'s completing thread hostage.
   *
   *  @tparam T the type of the value contained in the `Future`
   *  @param wrapped the Scala `Future` to wrap
   */
  final class CF[T](val wrapped: Future[T]) extends CompletableFuture[T] with (Try[T] => Unit) {
    /** Completes this `CompletableFuture` with the result of the given `Try`: called when
     *  the wrapped Scala `Future` completes, delegating to `complete` on `Success` and to
     *  `completeExceptionally` on `Failure`.
     *
     *  @param t the `Try` containing the result or exception to complete with
     */
    override def apply(t: Try[T]): Unit = t match {
      case Success(v) => complete(v)
      case Failure(e) => completeExceptionally(e)
    }

    // Ensure that completions of this future cannot hold the Scala Future's completer hostage

    /** Returns a new `CompletableFuture` that applies the given function to the result of this future.
     *
     *  @tparam U the type of the result of the function
     *  @param fn the function to apply to the result
     *  @return a new `CompletableFuture` that completes asynchronously with the result of applying the function
     */
    override def thenApply[U](fn: JFunction[? >: T, ? <: U]): CompletableFuture[U] = thenApplyAsync(fn)

    /** Returns a new `CompletableFuture` that consumes the result of this future with the given action.
     *
     *  @param fn the action to perform on the result
     *  @return a new `CompletableFuture` that completes asynchronously once the action has been performed
     */
    override def thenAccept(fn: Consumer[? >: T]): CompletableFuture[Void] = thenAcceptAsync(fn)

    /** Returns a new `CompletableFuture` that runs the given action when this future completes.
     *
     *  @param fn the action to run
     *  @return a new `CompletableFuture` that completes asynchronously once the action has been run
     */
    override def thenRun(fn: Runnable): CompletableFuture[Void] = thenRunAsync(fn)

    /** Returns a new `CompletableFuture` that combines the result of this future and the given `CompletionStage` using the provided function.
     *
     *  @tparam U the type of the result of the given `CompletionStage`
     *  @tparam V the type of the result of the combining function
     *  @param cs the `CompletionStage` to combine with
     *  @param fn the function to combine the results
     *  @return a new `CompletableFuture` that completes asynchronously with the result of the combining function
     */
    override def thenCombine[U, V](cs: CompletionStage[? <: U], fn: BiFunction[? >: T, ? >: U, ? <: V]): CompletableFuture[V] = thenCombineAsync(cs, fn)

    /** Returns a new `CompletableFuture` that consumes the results of this future and the given `CompletionStage` with the provided action.
     *
     *  @tparam U the type of the result of the given `CompletionStage`
     *  @param cs the `CompletionStage` to combine with
     *  @param fn the action to perform on the results
     *  @return a new `CompletableFuture` that completes asynchronously once the action has been performed
     */
    override def thenAcceptBoth[U](cs: CompletionStage[? <: U], fn: BiConsumer[? >: T, ? >: U]): CompletableFuture[Void] = thenAcceptBothAsync(cs, fn)

    /** Returns a new `CompletableFuture` that runs the given action when both this future and the given `CompletionStage` complete.
     *
     *  @param cs the `CompletionStage` to wait for
     *  @param fn the action to run
     *  @return a new `CompletableFuture` that completes asynchronously once the action has been run
     */
    override def runAfterBoth(cs: CompletionStage[?], fn: Runnable): CompletableFuture[Void] = runAfterBothAsync(cs, fn)

    /** Returns a new `CompletableFuture` that applies the given function to the result of whichever of this future or the given `CompletionStage` completes first.
     *
     *  @tparam U the type of the result of the function
     *  @param cs the `CompletionStage` to race with
     *  @param fn the function to apply to the result
     *  @return a new `CompletableFuture` that completes asynchronously with the result of applying the function
     */
    override def applyToEither[U](cs: CompletionStage[? <: T], fn: JFunction[? >: T, U]): CompletableFuture[U] = applyToEitherAsync(cs, fn)

    /** Returns a new `CompletableFuture` that consumes the result of whichever of this future or the given `CompletionStage` completes first with the given action.
     *
     *  @param cs the `CompletionStage` to race with
     *  @param fn the action to perform on the result
     *  @return a new `CompletableFuture` that completes asynchronously once the action has been performed
     */
    override def acceptEither(cs: CompletionStage[? <: T], fn: Consumer[? >: T]): CompletableFuture[Void] = acceptEitherAsync(cs, fn)

    /** Returns a new `CompletableFuture` that runs the given action when either this future or the given `CompletionStage` completes.
     *
     *  @param cs the `CompletionStage` to race with
     *  @param fn the action to run
     *  @return a new `CompletableFuture` that completes asynchronously once the action has been run
     */
    override def runAfterEither(cs: CompletionStage[?], fn: Runnable): CompletableFuture[Void] = runAfterEitherAsync(cs, fn)

    /** Returns a new `CompletableFuture` that completes with the result of the `CompletionStage` returned by the given function when applied to the result of this future.
     *
     *  @tparam U the type of the result of the `CompletionStage` returned by the function
     *  @param fn the function to apply to the result
     *  @return a new `CompletableFuture` that completes asynchronously with the result of the `CompletionStage` returned by the function
     */
    override def thenCompose[U](fn: JFunction[? >: T, ? <: CompletionStage[U]]): CompletableFuture[U] = thenComposeAsync(fn)

    /** Returns a new `CompletableFuture` that performs the given action when this future completes, whether normally or exceptionally.
     *
     *  @param fn the action to perform on the result or exception
     *  @return a new `CompletableFuture` that completes asynchronously with the same result as this future
     */
    override def whenComplete(fn: BiConsumer[? >: T, ? >: Throwable]): CompletableFuture[T] = whenCompleteAsync(fn)

    /** Returns a new `CompletableFuture` that applies the given function to the result or exception of this future.
     *
     *  @tparam U the type of the result of the function
     *  @param fn the function to apply to the result or exception
     *  @return a new `CompletableFuture` that completes asynchronously with the result of applying the function
     */
    override def handle[U](fn: BiFunction[? >: T, Throwable, ? <: U]): CompletableFuture[U] = handleAsync(fn)

    /** Returns a new `CompletableFuture` that completes with the result of applying the given function to the exception if this future completes exceptionally.
     *
     *  @param fn the function to apply to the exception
     *  @return a new `CompletableFuture` that completes with the result of applying the function,
     *          with the same result as this future if it completes normally, or exceptionally
     *          with the exception thrown by `fn` if `fn` itself throws
     */
    override def exceptionally(fn: JFunction[Throwable, ? <: T]): CompletableFuture[T] = {
      val cf = new CompletableFuture[T]
      whenCompleteAsync((t, e) => {
          if (e == null) cf.complete(t)
          else {
            val n: AnyRef =
              try {
                fn(e).asInstanceOf[AnyRef]
              } catch {
                case thr: Throwable =>
                  cf.completeExceptionally(thr)
                  this
              }
            if (n ne this) cf.complete(n.asInstanceOf[T])
          }
        }
      )
      cf
    }

    /**
      * @inheritdoc
      *
      * WARNING: completing the result of this method will not complete the underlying
      *          Scala Future or Promise (ie, the one that that was passed to `toJava`.)
      */
    override def toCompletableFuture: CompletableFuture[T] = this

    /** Throws `UnsupportedOperationException` because obtruding a value is not supported on the result of `toJava(scalaFuture)`.
     *
     *  @param value the value that would be obtruded, never used
     */
    override def obtrudeValue(value: T): Unit = throw new UnsupportedOperationException("obtrudeValue may not be used on the result of toJava(scalaFuture)")

    /** Throws `UnsupportedOperationException` because obtruding an exception is not supported on the result of `toJava(scalaFuture)`.
     *
     *  @param ex the exception that would be obtruded, never used
     */
    override def obtrudeException(ex: Throwable): Unit = throw new UnsupportedOperationException("obtrudeException may not be used on the result of toJava(scalaFuture)")

    /** Returns the result of this future, blocking if necessary until it is ready. The wait
     *  is wrapped in `scala.concurrent.blocking` to notify the current `BlockContext`.
     */
    override def get(): T = scala.concurrent.blocking(super.get())

    /** Returns the result of this future, blocking if necessary until it is ready or the timeout
     *  expires. The wait is wrapped in `scala.concurrent.blocking` to notify the current
     *  `BlockContext`.
     *
     *  @param timeout the maximum time to wait
     *  @param unit the time unit of the timeout
     *  @return the result of this future
     *  @throws TimeoutException if the timeout elapses before this future completes
     */
    override def get(timeout: Long, unit: TimeUnit): T = scala.concurrent.blocking(super.get(timeout, unit))

    /** Returns a string representation of this `CompletableFuture`.
     */
    override def toString(): String = super[CompletableFuture].toString
  }

  /** A `Promise` wrapper around a Java `CompletionStage`.
   *
   *  @tparam T the type of the value contained in the `CompletionStage`
   *  @param wrapped the Java `CompletionStage` to wrap
   */
  final class P[T](val wrapped: CompletionStage[T]) extends DefaultPromise[T] with BiFunction[T, Throwable, Unit] {
    /** Completes this `Promise` with the given value or exception.
     *
     *  @param v the value to complete with, or `null` if completing with an exception
     *  @param e the exception to complete with, or `null` if completing with a value
     */
    override def apply(v: T, e: Throwable): Unit = {
      if (e == null) success(v)
      else failure(e)
    }
  }
}

