package scala.runtime

import language.experimental.captureChecking
import java.util.concurrent.CountDownLatch

import scala.annotation.*

/**
 * Helper methods used in thread-safe lazy vals.
 */
object LazyVals {
  @nowarn
  private val unsafe: sun.misc.Unsafe = {
    def throwInitializationException() =
      throw new ExceptionInInitializerError(
        new IllegalStateException("Can't find instance of sun.misc.Unsafe")
      )
    try
      val unsafeField = classOf[sun.misc.Unsafe].getDeclaredField("theUnsafe").nn
      if unsafeField.getType == classOf[sun.misc.Unsafe] then
        unsafeField.setAccessible(true)
        unsafeField.get(null).asInstanceOf[sun.misc.Unsafe]
      else
        throwInitializationException()
    catch case _: NoSuchFieldException =>
      throwInitializationException()
  }

  private val base: Int = {
    val processors = java.lang.Runtime.getRuntime.availableProcessors()
    val rawSize = 8 * processors * processors
    //find the next power of 2
    1 << (32 - Integer.numberOfLeadingZeros(rawSize - 1))
  }

  private val mask: Int = base - 1

  private val monitors: Array[Object] =
    Array.tabulate(base)(_ => new Object)

  private def getMonitor(obj: Object, fieldId: Int = 0) = {
    monitors((java.lang.System.identityHashCode(obj) + fieldId) & mask)
  }

  private final val LAZY_VAL_MASK = 3L
  private final val debug = false

  /* ------------- Start of public API ------------- */

  // This trait extends Serializable to fix #16806 that caused a race condition
  /** A state stored in the field of a lazy val other than an ordinary computed value:
   *  [[Evaluating]] or a [[Waiting]] latch while the val is not yet bound, and
   *  [[NullValue]], which is the permanent sentinel for a lazy val that evaluated to
   *  `null`.
   *
   *  Extends `Serializable` so that an object can be serialized while one of
   *  its lazy vals holds a control state (issue #16806).
   */
  sealed trait LazyValControlState extends Serializable

  /**
   * Used to indicate the state of a lazy val that is being
   * evaluated and of which other threads await the result.
   */
  final class Waiting extends CountDownLatch(1) with LazyValControlState {
    /* #20856 If not fully evaluated yet, serialize as if not-evaluat*ing* yet.
     * This strategy ensures the "serializability" condition of parallel
     * programs--not to be confused with the data being `java.io.Serializable`.
     * Indeed, if thread A is evaluating the lazy val while thread B attempts
     * to serialize its owner object, there is also an alternative schedule
     * where thread B serializes the owner object *before* A starts evaluating
     * the lazy val. Therefore, forcing B to see the non-evaluating state is
     * correct.
     */
    private def writeReplace(): Any = null
  }

  /**
   * Used to indicate the state of a lazy val that is currently being
   * evaluated with no other thread awaiting its result.
   */
  object Evaluating extends LazyValControlState {
    /* #20856 If not fully evaluated yet, serialize as if not-evaluat*ing* yet.
     * See longer comment in `Waiting.writeReplace()`.
     */
    private def writeReplace(): Any = null
  }

  /**
   * Used to indicate the state of a lazy val that has been evaluated to
   * `null`.
   */
  object NullValue extends LazyValControlState

  /** The number of bits each lazy val occupies in the bitmap field of its
   *  enclosing class, encoding one of four states.
   */
  final val BITS_PER_LAZY_VAL = 2L

  /** Returns the state of the lazy val with ordinal `ord`, extracted from
   *  the bitmap value `cur`.
   *
   *  @param cur the current value of the bitmap field
   *  @param ord the ordinal of the lazy val within its bitmap
   *  @return the 2-bit state, `0` to `3`
   */
  def STATE(cur: Long, ord: Int): Long = {
    val r = (cur >> (ord * BITS_PER_LAZY_VAL)) & LAZY_VAL_MASK
    if (debug)
      println(s"STATE($cur, $ord) = $r")
    r
  }

  /** Attempts to atomically set the state of the lazy val with ordinal `ord`
   *  to `v`, expecting the bitmap field at `offset` in `t` to still hold `e`.
   *
   *  The proposed bitmap value is `e` with the 2-bit slot of `ord` replaced
   *  by `v`; the swap fails if the field no longer holds `e`.
   *
   *  @param t the object containing the bitmap field
   *  @param offset the memory offset of the bitmap field in `t`
   *  @param e the expected current value of the bitmap field
   *  @param v the new state for the lazy val
   *  @param ord the ordinal of the lazy val within its bitmap
   *  @return `true` if the swap succeeded, `false` if the field changed
   */
  def CAS(t: Object, offset: Long, e: Long, v: Int, ord: Int): Boolean = {
    if (debug)
      println(s"CAS($t, $offset, $e, $v, $ord)")
    val mask = ~(LAZY_VAL_MASK << ord * BITS_PER_LAZY_VAL)
    val n = (e & mask) | (v.toLong << (ord * BITS_PER_LAZY_VAL))
    unsafe.compareAndSwapLong(t, offset, e, n): @nowarn("cat=deprecation")
  }

  /** Attempts to atomically replace the contents of the object field at
   *  `offset` in `t` with `n`, expecting the field to still hold `exp`.
   *
   *  Used for lazy vals kept in a single object field that holds either the
   *  computed value or a [[LazyValControlState]].
   *
   *  @param t the object containing the field
   *  @param offset the memory offset of the field in `t`
   *  @param exp the expected current value, compared by reference
   *  @param n the value to store
   *  @return `true` if the swap succeeded, `false` if the field changed
   */
  def objCAS(t: Object, offset: Long, exp: Object, n: Object): Boolean = {
    if (debug)
      println(s"objCAS($t, $exp, $n)")
    unsafe.compareAndSwapObject(t, offset, exp, n): @nowarn("cat=deprecation")
  }

  /** Sets the state of the lazy val with ordinal `ord` to `v`, retrying the
   *  compare-and-swap until it succeeds.
   *
   *  If the state before the update was `2` (evaluating with waiting
   *  threads), notifies all threads blocked in [[wait4Notification]] on the
   *  val's monitor.
   *
   *  @param t the object containing the bitmap field
   *  @param offset the memory offset of the bitmap field in `t`
   *  @param v the new state for the lazy val
   *  @param ord the ordinal of the lazy val within its bitmap
   */
  def setFlag(t: Object, offset: Long, v: Int, ord: Int): Unit = {
    if (debug)
      println(s"setFlag($t, $offset, $v, $ord)")
    var retry = true
    while (retry) {
      val cur = get(t, offset)
      if (STATE(cur, ord) == 1) retry = !CAS(t, offset, cur, v, ord)
      else {
        // cur == 2, somebody is waiting on monitor
        if (CAS(t, offset, cur, v, ord)) {
          val monitor = getMonitor(t, ord)
          monitor.synchronized {
            monitor.notifyAll()
          }
          retry = false
        }
      }
    }
  }

  /** Blocks until the lazy val with ordinal `ord` leaves the evaluating
   *  states, that is, until the evaluating thread completes it via
   *  [[setFlag]].
   *
   *  Moves state `1` (evaluating, no waiting threads) to `2` (evaluating
   *  with waiting threads), then waits on the val's monitor while the state
   *  remains `2`; returns once the state is neither `1` nor `2`.
   *
   *  @param t the object containing the bitmap field
   *  @param offset the memory offset of the bitmap field in `t`
   *  @param cur the bitmap value the caller last read; used only for debug
   *             logging, the current value is re-read on each retry
   *  @param ord the ordinal of the lazy val within its bitmap
   */
  def wait4Notification(t: Object, offset: Long, cur: Long, ord: Int): Unit = {
    if (debug)
      println(s"wait4Notification($t, $offset, $cur, $ord)")
    var retry = true
    while (retry) {
      val cur = get(t, offset)
      val state = STATE(cur, ord)
      if (state == 1) CAS(t, offset, cur, 2, ord)
      else if (state == 2) {
        val monitor = getMonitor(t, ord)
        monitor.synchronized {
          if (STATE(get(t, offset), ord) == 2) // make sure notification did not happen yet.
            monitor.wait()
        }
      }
      else retry = false
    }
  }

  /** Returns the current value of the bitmap field at `off` in `t`, read
   *  with volatile semantics.
   *
   *  @param t the object containing the bitmap field
   *  @param off the memory offset of the bitmap field in `t`
   *  @return the bitmap value holding the states of the lazy vals
   */
  def get(t: Object, off: Long): Long = {
    if (debug)
      println(s"get($t, $off)")
    unsafe.getLongVolatile(t, off): @nowarn("cat=deprecation")
  }

  // kept for backward compatibility
  /** Returns the memory offset of the field named `name` declared in class
   *  `clz`.
   *
   *  @param clz the class declaring the field
   *  @param name the name of the field
   *  @return the offset, suitable for the `offset` arguments of the other
   *          members of this object
   *  @throws NoSuchFieldException if `clz` declares no field named `name`
   */
  def getOffset(clz: Class[?], name: String): Long = {
    @nowarn
    val r = unsafe.objectFieldOffset(clz.getDeclaredField(name))
    if (debug)
      println(s"getOffset($clz, $name) = $r")
    r
  }

  /** Returns the memory offset of the given static field within the static
   *  storage of its declaring class.
   *
   *  @param field the static field
   *  @return the offset, suitable for the `offset` arguments of the other
   *          members of this object
   */
  def getStaticFieldOffset(field: java.lang.reflect.Field): Long = {
    @nowarn
    val r = unsafe.staticFieldOffset(field)
    if (debug)
      println(s"getStaticFieldOffset(${field.getDeclaringClass}, ${field.getName}) = $r")
    r
  }

  /** Returns the memory offset of the given instance field within instances
   *  of its declaring class, suitable for the `offset` arguments of the
   *  other members of this object.
   *
   *  @param field the instance field
   */
  def getOffsetStatic(field: java.lang.reflect.Field) =
    @nowarn
    val r = unsafe.objectFieldOffset(field)
    if (debug)
      println(s"getOffset(${field.getDeclaringClass}, ${field.getName}) = $r")
    r


  object Names {
    /** The name of the [[STATE]] method, for use by code generators. */
    final val state = "STATE"
    /** The name of the [[CAS]] method, for use by code generators. */
    final val cas = "CAS"
    /** The name of the [[setFlag]] method, for use by code generators. */
    final val setFlag = "setFlag"
    /** The name of the [[wait4Notification]] method, for use by code generators. */
    final val wait4Notification = "wait4Notification"
    /** The name of the [[get]] method, for use by code generators. */
    final val get = "get"
    /** The name of the [[getOffset]] method, for use by code generators. */
    final val getOffset = "getOffset"
  }
}
