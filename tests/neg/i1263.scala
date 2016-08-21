object Test {
  trait Foo(val s: String)

  val foo1 = new Foo("bar") {}
  val foo2 = new Foo { override val s = "bar" } // error: parameterized trait lacks argument list
  def main(args: Array[String]): Unit = {
    assert(foo1.s == "bar")
    assert(foo2.s == "bar")
  }
}
object Test1 {
  trait Foo(private val s0: String) {
    def s = s0
  }

  val foo1 = new Foo("bar") {}
  def main(args: Array[String]): Unit = {
    assert(foo1.s == "bar")
  }
}
object Test2 {
  trait Foo(protected val s: String)

  val foo1 = new Foo("bar") {}
}
object Test3 {
  trait Foo(final val s: String)

  val foo1 = new Foo("bar") {}
  def main(args: Array[String]): Unit = {
    assert(foo1.s == "bar")
  }
}
