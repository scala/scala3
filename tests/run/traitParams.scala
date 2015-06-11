trait T(x: Int, val y: Int) {
  def f = x
}

trait U extends T {
  override def f = super.f + y
}

class C(x: Int) extends U with T(x, x * x)

class D extends C(10) with T {

}

object Test {
  def main(args: Array[String]): Unit =
    assert(new D().f == 110)
}

