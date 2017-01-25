class A {
  val a = "a"
  trait Inner {
    def f = println(a)
    def h = 3
  }
}

class Inner extends Test.a.Inner

object Test {
  val a = new A

  def main(args: Array[String]): Unit = {
    (new Inner).f
  }
}
