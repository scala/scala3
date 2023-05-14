// scalajs: --skip

import java.lang.ref.WeakReference
import java.util.concurrent.atomic.AtomicReference

final class Mark

object Test:

  def main(args: Array[String]): Unit =
    myTest
    trying

  final def myAssert(cond: => Boolean): Unit = assert(cond)

  def terminally(cond: => Boolean): Unit =
    System.gc()
    var n = 10
    while (n > 0 && !cond)
    do
      System.gc()
      Thread.`yield`()
      //print(".")
      n -= 1
    assert(cond)

  def myTest: Unit =
    val ref = new AtomicReference[WeakReference[AnyRef]]
    var mark: AnyRef = null
    assert(ref.compareAndSet(null, WeakReference(Mark())))
    mark = ref.get().get()
    myAssert(mark ne null) // in theory this could fail, but it isn't
    mark = null
    terminally(ref.get().get() == null)

  def trying: Unit =
    def ignore[A]: (Throwable => A) = _ => null.asInstanceOf[A]
    var i: Int = 21
    var s: String = "hello"
    var r: WeakReference[String] = null
    def f(n: => Int) = n + n + 1
    def g(x: => String) =
      r = WeakReference(x + "/" + x)
      r.get()
    i = try f(i) catch ignore
    s = try g(s) catch ignore
    assert(s == "hello/hello")
    assert(r.get() == "hello/hello")
    s = null
    terminally(r.get() == null)
    s = "bye"
    s = try g(s) catch ignore
    assert(s == "bye/bye")
    assert(r.get() == "bye/bye")
    s = null.asInstanceOf[String]
    terminally(r.get() == null)
    @volatile var z: String = "whoa"
    z = try g(z) catch ignore
    assert(r.get() == "whoa/whoa")
    z = null
    terminally(r.get() == null)
