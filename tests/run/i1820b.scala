trait A {
  val a = "a"
  trait Inner {
    def f = println(a)
    def h = 3
  }
}

trait B extends A {
  trait Inner2 extends Inner
  def g = new Inner2 {}
}

object Test {
  def main(args: Array[String]): Unit = {
    val b = new B {}
    b.g.f
  }
}
