class A {
  val a = "a"
  trait Inner {
    def f = println(a)
    def h = 3
  }
}

class Inner extends O.a.Inner

object O {
  val a = new A

  def main(args: Array[String]): Unit = {
    (new Inner).f
  }
}
