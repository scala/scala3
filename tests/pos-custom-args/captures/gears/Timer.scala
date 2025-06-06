package gears.async

import language.experimental.captureChecking

import gears.async.Listener

import java.util.concurrent.CancellationException
import java.util.concurrent.TimeoutException
import scala.annotation.tailrec
import scala.collection.mutable
import scala.concurrent.TimeoutException
import scala.concurrent.duration._
import scala.util.{Failure, Success, Try}
import scala.annotation.unchecked.uncheckedCaptures

import AsyncOperations.sleep
import Future.Promise


/** Timer exposes a steady [[Async.Source]] of ticks that happens every `tickDuration` milliseconds. Note that the timer
  * does not start ticking until `start` is called (which is a blocking operation, until the timer is cancelled).
  *
  * You might want to manually `cancel` the timer, so that it gets garbage collected (before the enclosing [[Async]]
  * scope ends).
  */
class Timer(tickDuration: Duration) extends Cancellable {
  enum TimerEvent:
    case Tick
    case Cancelled

  private var isCancelled = false

  private object Source extends Async.OriginalSource[this.TimerEvent] {
    private val listeners : mutable.Set[(Listener[TimerEvent]^) @uncheckedCaptures] =
      mutable.Set[(Listener[TimerEvent]^) @uncheckedCaptures]()

    def tick(): Unit = synchronized {
      listeners.filterInPlace(l =>
        l.completeNow(TimerEvent.Tick, src)
        false
      )
    }
    override def poll(k: Listener[TimerEvent]^): Boolean =
      if isCancelled then k.completeNow(TimerEvent.Cancelled, this)
      else false // subscribing to a timer always takes you to the next tick
    override def dropListener(k: Listener[TimerEvent]^): Unit = listeners -= k
    override protected def addListener(k: Listener[TimerEvent]^): Unit =
      if isCancelled then k.completeNow(TimerEvent.Cancelled, this)
      else
        Timer.this.synchronized:
          if isCancelled then k.completeNow(TimerEvent.Cancelled, this)
          else listeners += k

    def cancel(): Unit =
      synchronized { isCancelled = true }
      src.synchronized {
        Source.listeners.foreach(_.completeNow(TimerEvent.Cancelled, src))
        Source.listeners.clear()
      }
  }

  /** Ticks of the timer are delivered through this source. Note that ticks are ephemeral. */
  inline final def src: Async.Source[this.TimerEvent] = Source

  /** Starts the timer. Suspends until the timer is cancelled. */
  def run()(using Async, AsyncOperations): Unit =
    cancellationScope(this):
      loop()

  @tailrec private def loop()(using Async, AsyncOperations): Unit =
    if !isCancelled then
      try sleep(tickDuration.toMillis)
      catch case _: CancellationException => cancel()
    if !isCancelled then
      Source.tick()
      loop()

  override def cancel(): Unit = Source.cancel()
}

