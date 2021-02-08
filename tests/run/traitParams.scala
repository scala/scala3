object State {
  var s: Int = 0
}

trait T(x: Int, val y: Int) {
  def f = x
}

trait U extends T {
  State.s += 1
  override def f = super.f + y
}
trait U2(a: Any) extends T {
  def d = a // okay
  val v = a // okay
  a // used to crash
}

import State.*
class C(x: Int) extends U with T(x, x * x + s)
class C2(x: Int) extends T(x, x * x + s) with U

class D extends C(10) with T
class D2 extends C2(10) with T

object Test {
  def main(args: Array[String]): Unit = {
    assert(new D().f == 110)
    assert(new D2().f == 111)
  }
}

