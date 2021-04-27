/*
 * Scala.js (https://www.scala-js.org/)
 *
 * Copyright EPFL.
 *
 * Licensed under Apache License 2.0
 * (https://www.apache.org/licenses/LICENSE-2.0).
 *
 * See the NOTICE file distributed with this work for
 * additional information regarding copyright ownership.
 */

package org.scalajs.testsuite.jsinterop

import scala.scalajs.js
import scala.scalajs.js.annotation._
import scala.scalajs.js.|

import js.Thenable

object PromiseMock {

  @noinline
  def withMockedPromise[A](body: (() => Unit) => A): A = {
    val global = org.scalajs.testsuite.utils.JSUtils.globalObject

    val oldPromise =
      if (global.hasOwnProperty("Promise").asInstanceOf[Boolean]) Some(global.Promise)
      else None

    global.Promise = js.constructorOf[MockPromise[_]]
    try {
      body(MockPromise.processQueue _)
    } finally {
      oldPromise.fold {
        js.special.delete(global, "Promise")
      } { old =>
        global.Promise = old
      }
    }
  }

  @noinline
  def withMockedPromiseIfExists[A](body: (Option[() => Unit]) => A): A = {
    val global = org.scalajs.testsuite.utils.JSUtils.globalObject

    val oldPromise = global.Promise

    if (js.isUndefined(oldPromise)) {
      body(None)
    } else {
      global.Promise = js.constructorOf[MockPromise[_]]
      try {
        body(Some(MockPromise.processQueue _))
      } finally {
        global.Promise = oldPromise
      }
    }
  }

  private object MockPromise {
    private val queue = js.Array[js.Function0[Any]]()

    @JSExportStatic
    def resolve[A](value: A | js.Thenable[A]): MockPromise[A] = {
      new MockPromise[A]({
        (resolve: js.Function1[A | js.Thenable[A], _],
            reject: js.Function1[Any, _]) =>
          resolve(value)
      })
    }

    @JSExportStatic
    def reject(reason: Any): MockPromise[Nothing] = {
      new MockPromise[Nothing]({
        (resolve: js.Function1[Nothing | js.Thenable[Nothing], _],
            reject: js.Function1[Any, _]) =>
          reject(reason)
      })
    }

    def enqueue(f: js.Function0[Any]): Unit =
      queue.push(f)

    def processQueue(): Unit = {
      while (queue.nonEmpty)
        queue.shift()()
    }

    private sealed abstract class State[+A]

    private case object Pending extends State[Nothing]
    private case class Fulfilled[+A](value: A) extends State[A]
    private case class Rejected(reason: Any) extends State[Nothing]

    private def isNotAnObject(x: Any): Boolean = x match {
      case null | () | _:Double | _:Boolean | _:String => true
      case _                                           => false
    }

    private def isCallable(x: Any): Boolean =
      js.typeOf(x.asInstanceOf[js.Any]) == "function"

    private def throwAny(e: Any): Nothing = {
      throw (e match {
        case th: Throwable => th
        case _             => js.JavaScriptException(e)
      })
    }

    private def tryCatchAny[A](tryBody: => A)(catchBody: Any => A): A = {
      try {
        tryBody
      } catch {
        case th: Throwable =>
          catchBody(th match {
            case js.JavaScriptException(e) => e
            case _                         => th
          })
      }
    }
  }

  private class MockPromise[+A](
      executor: js.Function2[js.Function1[A | Thenable[A], _], js.Function1[scala.Any, _], _])
      extends js.Object with js.Thenable[A] {

    import MockPromise._

    private[this] var state: State[A] = Pending

    private[this] var fulfillReactions = js.Array[js.Function1[A, Any]]()
    private[this] var rejectReactions = js.Array[js.Function1[Any, Any]]()

    init(executor)

    // 25.4.3.1 Promise(executor)
    private[this] def init(
        executor: js.Function2[js.Function1[A | Thenable[A], _], js.Function1[scala.Any, _], _]) = {
      tryCatchAny[Unit] {
        executor(resolve _, reject _)
      } { e =>
        reject(e)
      }
    }

    private[this] def fulfill(value: A): Unit = {
      assert(state == Pending)
      state = Fulfilled(value)
      clearAndTriggerReactions(fulfillReactions, value)
    }

    private[this] def clearAndTriggerReactions[A](
        reactions: js.Array[js.Function1[A, Any]],
        argument: A): Unit = {

      assert(state != Pending)

      fulfillReactions = null
      rejectReactions = null

      for (reaction <- reactions)
        enqueue(() => reaction(argument))
    }

    // 25.4.1.3.2 Promise Resolve Functions
    private[this] def resolve(resolution: A | Thenable[A]): Unit = {
      if (state == Pending) {
        if (resolution.asInstanceOf[AnyRef] eq this) {
          reject(new js.TypeError("Self resolution"))
        } else if (isNotAnObject(resolution)) {
          fulfill(resolution.asInstanceOf[A])
        } else {
          tryCatchAny {
            val thenAction = resolution.asInstanceOf[js.Dynamic].`then`
            if (!isCallable(thenAction)) {
              fulfill(resolution.asInstanceOf[A])
            } else {
              val thenable = resolution.asInstanceOf[Thenable[A]]
              val thenActionFun = thenAction.asInstanceOf[js.Function]
              enqueue(() => promiseResolveThenableJob(thenable, thenActionFun))
            }
          } { e =>
            reject(e)
          }
        }
      }
    }

    // 25.4.2.2 PromiseResolveThenableJob
    private[this] def promiseResolveThenableJob(thenable: Thenable[A],
        thenAction: js.Function): Unit = {
      thenAction.call(thenable, resolve _, reject _)
    }

    // 25.4.1.3.1 Promise Reject Functions
    private[this] def reject(reason: Any): Unit = {
      if (state == Pending) {
        state = Rejected(reason)
        clearAndTriggerReactions(rejectReactions, reason)
      }
    }

    // 25.4.5.3 Promise.prototype.then
    def `then`[B](
        onFulfilled: js.Function1[A, B | Thenable[B]],
        onRejected: js.UndefOr[js.Function1[scala.Any, B | Thenable[B]]]): MockPromise[B] = {

      new MockPromise[B](
        { (innerResolve: js.Function1[B | Thenable[B], _],
            innerReject: js.Function1[scala.Any, _]) =>

          def doFulfilled(value: A): Unit = {
            tryCatchAny[Unit] {
              innerResolve(onFulfilled(value))
            } { e =>
              innerReject(e)
            }
          }

          def doRejected(reason: Any): Unit = {
            tryCatchAny[Unit] {
              onRejected.fold[Unit] {
                innerReject(reason)
              } { onRejectedFun =>
                innerResolve(onRejectedFun(reason))
              }
            } { e =>
              innerReject(e)
            }
          }

          state match {
            case Pending =>
              fulfillReactions += doFulfilled _
              rejectReactions += doRejected _

            case Fulfilled(value) =>
              enqueue(() => doFulfilled(value))

            case Rejected(reason) =>
              enqueue(() => doRejected(reason))
          }
        }
      )
    }

    def `then`[B >: A](
        onFulfilled: Unit,
        onRejected: js.UndefOr[js.Function1[scala.Any, B | Thenable[B]]]): MockPromise[B] = {
      `then`((x: A) => (x: B | Thenable[B]), onRejected)
    }

    // 25.4.5.1 Promise.prototype.catch
    def `catch`[B >: A](
        onRejected: js.UndefOr[js.Function1[scala.Any, B | Thenable[B]]]): MockPromise[B] = {
      `then`((), onRejected)
    }
  }
}
