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
  final class CF[T](val wrapped: Future[T]) extends CompletableFuture[T] with (Try[T] => Unit) {
    override def apply(t: Try[T]): Unit = t match {
      case Success(v) => complete(v)
      case Failure(e) => completeExceptionally(e)
    }

    // Ensure that completions of this future cannot hold the Scala Future's completer hostage

    override def thenApply[U](fn: JFunction[? >: T, ? <: U]): CompletableFuture[U] = thenApplyAsync(fn)

    override def thenAccept(fn: Consumer[? >: T]): CompletableFuture[Void] = thenAcceptAsync(fn)

    override def thenRun(fn: Runnable): CompletableFuture[Void] = thenRunAsync(fn)

    override def thenCombine[U, V](cs: CompletionStage[? <: U], fn: BiFunction[? >: T, ? >: U, ? <: V]): CompletableFuture[V] = thenCombineAsync(cs, fn)

    override def thenAcceptBoth[U](cs: CompletionStage[? <: U], fn: BiConsumer[? >: T, ? >: U]): CompletableFuture[Void] = thenAcceptBothAsync(cs, fn)

    override def runAfterBoth(cs: CompletionStage[?], fn: Runnable): CompletableFuture[Void] = runAfterBothAsync(cs, fn)

    override def applyToEither[U](cs: CompletionStage[? <: T], fn: JFunction[? >: T, U]): CompletableFuture[U] = applyToEitherAsync(cs, fn)

    override def acceptEither(cs: CompletionStage[? <: T], fn: Consumer[? >: T]): CompletableFuture[Void] = acceptEitherAsync(cs, fn)

    override def runAfterEither(cs: CompletionStage[?], fn: Runnable): CompletableFuture[Void] = runAfterEitherAsync(cs, fn)

    override def thenCompose[U](fn: JFunction[? >: T, ? <: CompletionStage[U]]): CompletableFuture[U] = thenComposeAsync(fn)

    override def whenComplete(fn: BiConsumer[? >: T, ? >: Throwable]): CompletableFuture[T] = whenCompleteAsync(fn)

    override def handle[U](fn: BiFunction[? >: T, Throwable, ? <: U]): CompletableFuture[U] = handleAsync(fn)

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

    override def obtrudeValue(value: T): Unit = throw new UnsupportedOperationException("obtrudeValue may not be used on the result of toJava(scalaFuture)")

    override def obtrudeException(ex: Throwable): Unit = throw new UnsupportedOperationException("obtrudeException may not be used on the result of toJava(scalaFuture)")

    override def get(): T = scala.concurrent.blocking(super.get())

    override def get(timeout: Long, unit: TimeUnit): T = scala.concurrent.blocking(super.get(timeout, unit))

    override def toString(): String = super[CompletableFuture].toString
  }

  final class P[T](val wrapped: CompletionStage[T]) extends DefaultPromise[T] with BiFunction[T, Throwable, Unit] {
    override def apply(v: T, e: Throwable): Unit = {
      if (e == null) success(v)
      else failure(e)
    }
  }
}

