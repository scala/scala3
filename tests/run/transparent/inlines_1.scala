package p
import collection.mutable

object transparents {

  final val monitored = false

  transparent def f(x: Int): Int = x * x

  val hits = new mutable.HashMap[String, Int] {
    override def default(key: String): Int = 0
  }

  def record(fn: String, n: Int = 1) = {
    if (monitored) {
      val name = if (fn.startsWith("member-")) "member" else fn
      hits(name) += n
    }
  }

  @volatile private var stack: List[String] = Nil

  transparent def track[T](fn: String)(op: => T) =
    if (monitored) {
      stack = fn :: stack
      record(fn)
      try op
      finally stack = stack.tail
    } else op

  class Outer {
    def f = "Outer.f"
    class Inner {
      val msg = " Inner"
      transparent def m = msg
      transparent def g = f
      transparent def h = f ++ m
    }
    val inner = new Inner
  }
}
