object Test {
  trait Foo(val s: String)

  val foo1 = new Foo("bar") {}
  val foo2 = new Foo { override val s = "bar" }
  def main(args: Array[String]): Unit = {
    assert(foo1.s == "bar")
    assert(foo2.s == "bar")
  }
}
