package gears.async

import language.experimental.captureChecking

import scala.concurrent.duration._
import scala.annotation.capability

/** The delimited continuation suspension interface. Represents a suspended computation asking for a value of type `T`
  * to continue (and eventually returning a value of type `R`).
  */
trait Suspension[-T, +R]:
  def resume(arg: T): R

/** Support for suspension capabilities through a delimited continuation interface. */
trait SuspendSupport:
  /** A marker for the "limit" of "delimited continuation". */
  type Label[R, Cap^] <: caps.Capability

  /** The provided suspension type. */
  type Suspension[-T, +R] <: gears.async.Suspension[T, R]

  /** Set the suspension marker as the body's caller, and execute `body`. */
  def boundary[R, Cap^](body: Label[R, Cap] ?->{Cap^} R): R^{Cap^}

  /** Should return immediately if resume is called from within body */
  def suspend[T, R, Cap^](body: Suspension[T, R]^{Cap^} => R^{Cap^})(using Label[R, Cap]): T

/** Extends [[SuspendSupport]] with "asynchronous" boundary/resume functions, in the presence of a [[Scheduler]] */
trait AsyncSupport extends SuspendSupport:
  type Scheduler <: gears.async.Scheduler

  /** Resume a [[Suspension]] at some point in the future, scheduled by the scheduler. */
  private[async] def resumeAsync[T, R](suspension: Suspension[T, R])(arg: T)(using s: Scheduler): Unit =
    s.execute(() => suspension.resume(arg))

  /** Schedule a computation with the suspension boundary already created. */
  private[async] def scheduleBoundary[Cap^](body: Label[Unit, Cap] ?-> Unit)(using s: Scheduler): Unit =
    s.execute(() => boundary[Unit, Cap](body))

/** A scheduler implementation, with the ability to execute a computation immediately or after a delay. */
trait Scheduler:
  def execute(body: Runnable): Unit
  def schedule(delay: FiniteDuration, body: Runnable): Cancellable

object AsyncSupport:
  inline def apply()(using ac: AsyncSupport) = ac
