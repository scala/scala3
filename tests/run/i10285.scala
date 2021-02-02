trait Companion {

  trait AbstractFoo {
    val a0 = 1
    private val b0 = 2
    def b1 = b0
    val (a, b) = {
      val av  = 1
      val bv  = 2
      (av, bv)
    }
  }
}

object Bar extends Companion {
  class FooImpl extends AbstractFoo
  val foo = new FooImpl()
}

object Test {
  val foo = new Bar.FooImpl
  def main(args: Array[String]): Unit =
    assert(Bar.foo.a0 + Bar.foo.b1 == 3)
    assert(Bar.foo.a + Bar.foo.b == 3)
}