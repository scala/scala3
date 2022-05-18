package scala.runtime

/**
 * Helper methods used in thread-safe lazy vals.
 */
object LazyVals {
  private[this] val unsafe: sun.misc.Unsafe =
      classOf[sun.misc.Unsafe].getDeclaredFields.nn.find { field =>
        field.nn.getType == classOf[sun.misc.Unsafe] && {
          field.nn.setAccessible(true)
          true
        }
      }
      .map(_.nn.get(null).asInstanceOf[sun.misc.Unsafe])
      .getOrElse {
        throw new ExceptionInInitializerError {
          new IllegalStateException("Can't find instance of sun.misc.Unsafe")
        }
      }

  private[this] val base: Int = {
    val processors = java.lang.Runtime.getRuntime.nn.availableProcessors()
    8 * processors * processors
  }

  private[this] val monitors: Array[Object] =
    Array.tabulate(base)(_ => new Object)

  private def getMonitor(obj: Object, fieldId: Int = 0) = {
    var id = (java.lang.System.identityHashCode(obj) + fieldId) % base

    if (id < 0) id += base
    monitors(id)
  }

  private final val LAZY_VAL_MASK = 3L
  private final val debug = false

  /* ------------- Start of public API ------------- */

  /**
   * Used to indicate the state of a lazy val that is being
   * evaluated and of which other threads await the result.
   */
  final class Waiting:
    private var done = false
    
    /**
     * Wakes up waiting threads. Called on completion of the evaluation
     * of lazy val's right-hand side.
     */
    def release(): Unit = synchronized {
      done = true
      notifyAll()
    }

    /**
     * Awaits the completion of the evaluation of lazy val's right-hand side.
     */
    def awaitRelease(): Unit = synchronized {
      while !done do wait()
    }

  /**
   * Used to indicate the state of a lazy val that is currently being
   * evaluated with no other thread awaiting its result.
   */
  object Evaluating

  /**
   * Used to indicate the state of a lazy val that has been evaluated to
   * `null`.
   */
  object NULL

  final val BITS_PER_LAZY_VAL = 2L

  def STATE(cur: Long, ord: Int): Long = {
    val r = (cur >> (ord * BITS_PER_LAZY_VAL)) & LAZY_VAL_MASK
    if (debug)
      println(s"STATE($cur, $ord) = $r")
    r
  }

  def CAS(t: Object, offset: Long, e: Long, v: Int, ord: Int): Boolean = {
    if (debug)
      println(s"CAS($t, $offset, $e, $v, $ord)")
    val mask = ~(LAZY_VAL_MASK << ord * BITS_PER_LAZY_VAL)
    val n = (e & mask) | (v.toLong << (ord * BITS_PER_LAZY_VAL))
    unsafe.compareAndSwapLong(t, offset, e, n)
  }

  def objCAS(t: Object, offset: Long, exp: Object, n: Object): Boolean = {
    if (debug)
      println(s"objCAS($t, $exp, $n)")
    unsafe.compareAndSwapObject(t, offset, exp, n)
  }

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

  def get(t: Object, off: Long): Long = {
    if (debug)
      println(s"get($t, $off)")
    unsafe.getLongVolatile(t, off)
  }

  // kept for backward compatibility
  def getOffset(clz: Class[_], name: String): Long = {
    val r = unsafe.objectFieldOffset(clz.getDeclaredField(name))
    if (debug)
      println(s"getOffset($clz, $name) = $r")
    r
  }

  def getStaticFieldOffset(field: java.lang.reflect.Field): Long = {
    val r = unsafe.staticFieldOffset(field)
    if (debug)
      println(s"getStaticFieldOffset(${field.getDeclaringClass}, ${field.getName}) = $r")
    r
  }

  def getOffsetStatic(field: java.lang.reflect.Field) =
    val r = unsafe.objectFieldOffset(field)
    if (debug)
      println(s"getOffset(${field.getDeclaringClass}, ${field.getName}) = $r")
    r


  object Names {
    final val waiting = "Waiting"
    final val evaluating = "Evaluating"
    final val nullValued = "NULL"
    final val waitingAwaitRelease = "awaitRelease"
    final val waitingRelease = "release"
    final val state = "STATE"
    final val cas = "CAS"
    final val objCas = "objCAS"
    final val setFlag = "setFlag"
    final val wait4Notification = "wait4Notification"
    final val get = "get"
    final val getOffset = "getOffset"
    final val getOffsetStatic = "getOffsetStatic"
    final val getStaticFieldOffset = "getStaticFieldOffset"
  }
}
