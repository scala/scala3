object Test {
  case class Foo(name: String, children: Int *)

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

  def main(args: Array[String]): Unit = {
    val f = new Foo("hello", 3, 5)
    foo(f)
    bar(f)
  }
}
