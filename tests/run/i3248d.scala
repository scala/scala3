object Test {
  class Foo(val name: String, val children: Seq[Int])

  object Foo {
    def unapplySeq(foo: Foo): (String, Seq[Int]) = (foo.name, foo.children)
  }

  def foo(f: Foo) = f match {
    case Foo(name, ns: _*) =>
      assert(name == "hello")
      assert(ns(0) == 3)
      assert(ns(1) == 5)
  }

  def bar(f: Foo) = f match {
    case Foo(name, x, y, ns : _*) =>
      assert(name == "hello")
      assert(x == 3)
      assert(y == 5)
      assert(ns.isEmpty)
  }

  def qux(f: Foo) = f match {
    case Foo(name) => 1
    case Foo(name, x, y) => 2
    case Foo(name, xs: _*) => 0
  }


  def main(args: Array[String]): Unit = {
    val f = new Foo("hello", List(3, 5))
    foo(f)
    bar(f)
    assert(qux(new Foo("hello", Nil)) == 1)
    assert(qux(new Foo("hello", 5 :: 6 :: Nil)) == 2)
    assert(qux(new Foo("hello", 5 :: Nil)) == 0)
    assert(qux(new Foo("hello", 5 :: 6 :: 7 :: Nil)) == 0)
  }
}
