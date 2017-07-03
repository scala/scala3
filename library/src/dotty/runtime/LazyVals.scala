package dotty.runtime

import scala.annotation.tailrec

/**
 * Helper methods used in thread-safe lazy vals.
 */
object LazyVals {
  private val unsafe: sun.misc.Unsafe =
      classOf[sun.misc.Unsafe].getDeclaredFields.find { field =>
        field.getType == classOf[sun.misc.Unsafe] && {
          field.setAccessible(true)
          true
        }
      }
      .map(_.get(null).asInstanceOf[sun.misc.Unsafe])
      .getOrElse {
        throw new ExceptionInInitializerError {
          new IllegalStateException("Can't find instance of sun.misc.Unsafe")
        }
      }

  final val BITS_PER_LAZY_VAL = 2L
  final val LAZY_VAL_MASK = 3L
  final val debug = false

  @inline def STATE(cur: Long, ord: Int) = {
    val r = (cur >> (ord * BITS_PER_LAZY_VAL)) & LAZY_VAL_MASK
    if (debug)
      println(s"STATE($cur, $ord) = $r")
    r
  }
  @inline def CAS(t: Object, offset: Long, e: Long, v: Int, ord: Int) = {
    if (debug)
      println(s"CAS($t, $offset, $e, $v, $ord)")
    val mask = ~(LAZY_VAL_MASK << ord * BITS_PER_LAZY_VAL)
    val n = (e & mask) | (v << (ord * BITS_PER_LAZY_VAL))
    compareAndSet(t, offset, e, n)
  }
  @inline def setFlag(t: Object, offset: Long, v: Int, ord: Int) = {
    if (debug)
      println(s"setFlag($t, $offset, $v, $ord)")
    var retry = true
    while (retry) {
      val cur = get(t, offset)
      if (STATE(cur, ord) == 1) retry = CAS(t, offset, cur, v, ord)
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
  @inline def wait4Notification(t: Object, offset: Long, cur: Long, ord: Int) = {
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
          monitor.wait()
        }
      }
      else retry = false
    }
  }

  @inline def compareAndSet(t: Object, off: Long, e: Long, v: Long) = unsafe.compareAndSwapLong(t, off, e, v)
  @inline def get(t: Object, off: Long) = {
    if (debug)
      println(s"get($t, $off)")
    unsafe.getLongVolatile(t, off)
  }

  val processors: Int = java.lang.Runtime.getRuntime.availableProcessors()
  val base: Int = 8 * processors * processors
  val monitors: Array[Object] = (0 to base).map {
    x => new Object()
  }.toArray

  @inline def getMonitor(obj: Object, fieldId: Int = 0) = {
    var id = (
      /*java.lang.System.identityHashCode(obj) + */ // should be here, but #548
      fieldId) % base

    if (id < 0) id += base
    monitors(id)
  }

  @inline def getOffset(clz: Class[_], name: String) = {
    val r = unsafe.objectFieldOffset(clz.getDeclaredField(name))
    if (debug)
      println(s"getOffset($clz, $name) = $r")
    r
  }

  object Names {
    final val state = "STATE"
    final val cas = "CAS"
    final val setFlag = "setFlag"
    final val wait4Notification = "wait4Notification"
    final val compareAndSet = "compareAndSet"
    final val get = "get"
    final val getOffset = "getOffset"
  }
}
