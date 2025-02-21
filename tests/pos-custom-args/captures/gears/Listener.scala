package gears.async

import language.experimental.captureChecking

import gears.async.Async.Source
import gears.async.Async.SourceSymbol

import java.util.concurrent.locks.ReentrantLock
import scala.annotation.tailrec

/** A listener, representing an one-time value receiver of an [[Async.Source]].
  *
  * Most of the time listeners should involve only calling a receiver function, and can be created by [[Listener.apply]]
  * or [[Listener.acceptingListener]].
  *
  * However, should the listener want to attempt synchronization, it has to expose some locking-related interfaces. See
  * [[Listener.lock]].
  */
trait Listener[-T]:
  import Listener._

  /** Complete the listener with the given item, from the given source. **If the listener exposes a
    * [[Listener.ListenerLock]]**, it is required to acquire this lock before calling [[complete]]. This can also be
    * done conveniently with [[completeNow]]. For performance reasons, this condition is usually not checked and will
    * end up causing unexpected behavior if not satisfied.
    *
    * The listener must automatically release its own lock upon completion.
    */
  def complete(data: T, source: Async.SourceSymbol[T]): Unit

  /** Represents the exposed API for synchronization on listeners at receiving time. If the listener does not have any
    * form of synchronization, [[lock]] should be `null`.
    */
  val lock: (Listener.ListenerLock^) | Null

  /** Attempts to acquire locks and then calling [[complete]] with the given item and source. If locking fails,
    * [[releaseLock]] is automatically called.
    */
  def completeNow(data: T, source: Async.SourceSymbol[T]): Boolean =
    if acquireLock() then
      this.complete(data, source)
      true
    else false

  /** Release the listener's lock if it exists. */
  inline final def releaseLock(): Unit = if lock != null then lock.release()

  /** Attempts to lock the listener, if such a lock exists. Succeeds with `true` immediately if [[lock]] is `null`.
    */
  inline final def acquireLock(): Boolean =
    if lock != null then lock.acquire() else true

object Listener:
  /** A simple [[Listener]] that always accepts the item and sends it to the consumer. */
  /* inline bug */ def acceptingListener[T](consumer: (T, SourceSymbol[T]) => Unit): Listener[T]^{consumer} =
    new Listener[T]:
      val lock = null
      def complete(data: T, source: SourceSymbol[T]) = consumer(data, source)

  /** Returns a simple [[Listener]] that always accepts the item and sends it to the consumer. */
  def apply[T](consumer: (T, SourceSymbol[T]) => Unit): Listener[T]^{consumer} = acceptingListener(consumer)

  /** A special class of listener that forwards the inner listener through the given source. For purposes of
    * [[Async.Source.dropListener]] these listeners are compared for equality by the hash of the source and the inner
    * listener.
    */
  abstract case class ForwardingListener[-T](src: Async.Source[?]^, inner: Listener[?]^) extends Listener[T]

  object ForwardingListener:
    /** Creates an empty [[ForwardingListener]] for equality comparison. */
    def empty(src: Async.Source[?]^, inner: Listener[?]^): ForwardingListener[Any]^{src, inner} = new ForwardingListener[Any](src, inner):
      val lock = null
      override def complete(data: Any, source: SourceSymbol[Any]) = ???

  /** A lock required by a listener to be acquired before accepting values. Should there be multiple listeners that
    * needs to be locked at the same time, they should be locked by larger-number-first.
    *
    * Some implementations are provided for ease of implementations:
    *   - For custom listener implementations involving locks: [[NumberedLock]] provides uniquely numbered locks.
    *   - For source transformation implementations: [[withLock]] is a convenient `.map` for `[[ListenerLock]] | Null`.
    */
  trait ListenerLock:
    /** The assigned number of the lock. It is required that listeners that can be locked together to have different
      * [[selfNumber numbers]]. This requirement can be simply done by using a lock created using [[NumberedLock]].
      */
    val selfNumber: Long

    /** Attempt to lock the current [[ListenerLock]]. Locks are guaranteed to be held as short as possible.
      */
    def acquire(): Boolean

    /** Release the current lock. */
    def release(): Unit
  end ListenerLock

  /** Maps the lock of a listener, if it exists. */
  inline def withLock[T](listener: Listener[?])(inline body: ListenerLock => T): T | Null =
    listener.lock match
      case null            => null
      case l: ListenerLock => body(l)

  /** A helper instance that provides an uniquely numbered mutex. */
  trait NumberedLock:
    import NumberedLock._

    val number = listenerNumber.getAndIncrement()
    private val lock0 = ReentrantLock()

    protected def acquireLock() = lock0.lock()
    protected def releaseLock() = lock0.unlock()

  object NumberedLock:
    private val listenerNumber = java.util.concurrent.atomic.AtomicLong()
