// scalajs: --skip

/** A demonstrator for a new algorithm to handle lazy vals. The idea is that
 *  we use the field slot itself for all synchronization; there are no separate bitmaps
 *  or locks. The type of a field is always Object. The field goes through the following
 *  state changes:
 *
 *      null -> Evaluating -+--------------+-> Initialized
 *                          |              |
 *                          +--> Waiting --+
 *
 *  The states of a field are characterized as follows:
 *
 *      x == null         Nobody has evaluated the variable yet
 *      x == Evaluating   A thread has started evaluating
 *      x: Waiting        A thread has started evaluating and other threads are waiting
 *                        for the result
 *      otherwise         Variable is initialized
 *
 *
 *  A lazy val `x: A = rhs` is compiled to the following code scheme:

    private @volatile var _x: AnyRef = null

    @tailrec def x: A =
        _x match
        case current: A =>
            current
        case null =>
            if CAS(_x, null, Evaluating) then
                var result: A = null
                try
                    result = rhs
                    if result == null then result = NULL // drop if A is non-nullable
                finally
                    if !CAS(_x, Evaluating, result) then
                        val lock = _x.asInstanceOf[Waiting]
                        CAS(_x, lock, result)
                        lock.release()
            x
        case Evaluating =>
            CAS(x, Evaluating, new Waiting)
            x
        case current: Waiting =>
            _x = current.awaitRelease()
            x
        case NULL => null                                // drop if A is non-nullable

 *  The code makes use of the following runtime class:

    class Waiting:
        private var done = false
        def release(): Unit = synchronized:
            done = true
            notifyAll()

        def awaitRelease(): Unit = synchronized:
            while !done do wait()

 *  Note: The code assumes that the getter result type `A` is disjoint from the type
 *  of `Evaluating` and the `Waiting` class. If this is not the case (e.g. `A` is AnyRef),
 *  then the conditions in the match have to be re-ordered so that case `_x: A` becomes
 *  the final default case.
 *
 *  Cost analysis:
 *
 *    - 2 CAS on contention-free initialization
 *    - 0 or 1 CAS on first read in thread other than initializer thread, depending on
 *      whether cache has updated
 *    - no synchronization operations on reads after the first one
 *    - If there is contention, we see in addition
 *       - for the initializing thread: another CAS and a synchronized notifyAll
 *       - for a reading thread: 0 or 1 CAS and a synchronized wait
 *
 *  Code sizes for getter:
 *
 *   this scheme, if nulls are excluded in type: 86 bytes
 *   current Dotty scheme: 125 bytes
 *   Scala 2 scheme: 39 bytes
 *
 *  Advantages of the scheme:
 *
 *   - no slot other than the field itself is needed
 *   - no locks are shared among lazy val initializations and between lazy val initializations
 *     and normal code
 *   - no deadlocks (other than those inherent in user code)
 *   - synchronized code is executed only if there is contention
 *   - simpler than current Dotty scheme
 *
 *  Disadvantages:
 *
 *   - lazy vals of primitive types are boxed
 */
import sun.misc.Unsafe.*

class C {
  def init(name: String) = {
    Thread.sleep(10)
    println(s"initialize $name"); "result"
  }

  @volatile private[this] var _x: AnyRef = compiletime.uninitialized

  // Expansion of:  lazy val x: String = init("x")

  def x: String = {
    val current = _x
    if (current.isInstanceOf[String])
      current.asInstanceOf[String]
    else
      x$lzy
  }

  def x$lzy: String = {
    val current = _x
    if (current.isInstanceOf[String])
      current.asInstanceOf[String]
    else {
      val offset = C.x_offset
      if (current == null) {
        if (LazyRuntime.isUnitialized(this, offset)) {
          try LazyRuntime.initialize(this, offset, init("x"))
          catch {
            case ex: Throwable =>
              LazyRuntime.initialize(this, offset, null)
              throw ex
          }
        }
      }
      else
        LazyRuntime.awaitInitialized(this, offset, current)
      x$lzy
    }
  }

  // Compare with bytecodes for regular lazy val:
  lazy val y = init("y")
}

object C {
  import LazyRuntime.fieldOffset
  val x_offset = fieldOffset(classOf[C], "_x")
}

object LazyRuntime {
  val Evaluating = new LazyControl()

  private val unsafe: sun.misc.Unsafe = {
    val f: java.lang.reflect.Field = classOf[sun.misc.Unsafe].getDeclaredField("theUnsafe");
    f.setAccessible(true)
    f.get(null).asInstanceOf[sun.misc.Unsafe]
  }

  def fieldOffset(cls: Class[_], name: String): Long = {
    val fld = cls.getDeclaredField(name)
    fld.setAccessible(true)
    unsafe.objectFieldOffset(fld)
  }

  def isUnitialized(base: Object, offset: Long): Boolean =
    unsafe.compareAndSwapObject(base, offset, null, Evaluating)

  def initialize(base: Object, offset: Long, result: Object): Unit =
    if (!unsafe.compareAndSwapObject(base, offset, Evaluating, result)) {
      val lock = unsafe.getObject(base, offset).asInstanceOf[Waiting]
      unsafe.compareAndSwapObject(base, offset, lock, result)
      lock.release()
    }

  def awaitInitialized(base: Object, offset: Long, current: Object): Unit =
    if (current.isInstanceOf[Waiting])
      current.asInstanceOf[Waiting].awaitRelease()
    else
      unsafe.compareAndSwapObject(base, offset, Evaluating, new Waiting)
}

class LazyControl

class Waiting extends LazyControl {

  private var done = false

  def release(): Unit = synchronized {
    done = true
    notifyAll()
  }

  def awaitRelease(): Unit = synchronized {
    while (!done) wait()
  }
}

object Test {
  def main(args: Array[String]) = {
    val c = new C()
    println(c.x)
    println(c.x)
    println(c.y)
    multi()
  }

  def multi() = {
    val rand = java.util.Random()
    val c = new C()
    val readers =
      for i <- 0 until 500 yield
        new Thread {
          override def run() = {
            Thread.sleep(rand.nextInt(50))
            assert(c.x == "result")
          }
        }
    for (t <- readers) t.start()
    for (t <- readers) t.join()
  }
}
