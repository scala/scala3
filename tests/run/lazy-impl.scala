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
 *  Note 1: This assumes that fields cannot have `null` as normal value. Once we have
 *  nullability checking, this should be the standard case. We can still accommodate
 *  fields that can be null by representing `null` with a special value (say `NULL`)
 *  and storing `NULL` instead of `null` in the field. The necessary tweaks are added
 *  as comment lines to the code below.
 *
 *  A lazy val `x: A = rhs` is compiled to the following code scheme:
 *
 *  private var _x: AnyRef = null
 *  def x: A =
 *      _x match
 *      case current: A =>
 *          current
 *      case null =>
 *          if CAS(_x, null, Evaluating) then
 *              var result = rhs
 *  //          if result == null then result == NULL
 *              if !CAS(x, Evaluating, result) then
 *                  val lock = _x.asInstanceOf[Waiting]
 *                  _x = result
 *                  lock.release(result)
 *          x
 *      case Evaluating =>
 *          CAS(x, Evaluating, new Waiting)
 *          x
 *      case current: Waiting =>
 *          _x = current.awaitRelease()
 *          x
 *  //  case NULL =>
 *  //      null
 *

 *  The code makes use of the following runtime class:
 *
 *  class Waiting:
 *
 *      private var done = false
 *      private var result: AnyRef = _
 *
 *      def release(result: AnyRef): Unit = synchronized:
 *          this.result = result
 *          done = true
 *          notifyAll()
 *
 *      def awaitRelease(): AnyRef = synchronized:
 *          if !done then wait()
 *          result
 *
 *  Note 2: The code assumes that the getter result type `A` is disjoint from the type
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
 *       - for the initializing thread: a synchronized notifyAll
 *       - for a reading thread: 0 or 1 CAS and a synchronized wait
 *
 *  Code sizes for getter:
 *
 *   this scheme, if nulls are excluded in type: 72 bytes
 *   current Dotty scheme: 131 bytes
 *   Scala 2 scheme: 39 bytes + 1 exception handler
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
 *   - does not work for local lazy vals (but maybe these could be unsynchronized anyway?)
 *   - lazy vals of primitive types are boxed
 */
import sun.misc.Unsafe._

class C {
  def init(name: String) = {
    Thread.sleep(10)
    println(s"initialize $name"); "result"
  }

  private[this] var _x: AnyRef = null

  // Expansion of:  lazy val x: String = init

  def x: String = {
    val current = _x
    if (current.isInstanceOf[String])
      current.asInstanceOf[String]
    else
      x$lzy_compute
  }

  def x$lzy_compute: String = {
    val current = _x
    if (current.isInstanceOf[String])
      current.asInstanceOf[String]
    else {
      val offset = C.x_offset
      if (current == null) {
        if (LazyRuntime.isUnitialized(this, offset))
          LazyRuntime.initialize(this, offset, init("x"))
      }
      else
        LazyRuntime.awaitInitialized(this, offset, current)
      x$lzy_compute
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
      unsafe.putObject(base, offset, result)
      lock.release(result)
    }

  def awaitInitialized(base: Object, offset: Long, current: Object): Unit =
    if (current.isInstanceOf[Waiting])
      unsafe.putObject(base, offset, current.asInstanceOf[Waiting].awaitRelease())
    else
      unsafe.compareAndSwapObject(base, offset, Evaluating, new Waiting)
}

class LazyControl

class Waiting extends LazyControl {

  private var done = false
  private var result: AnyRef = _

  def release(result: AnyRef) = synchronized {
    this.result = result
    done = true
    notifyAll()
  }

  def awaitRelease(): AnyRef = synchronized {
    if (!done) wait()
    result
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
      for i <- 0 until 1000 yield
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
