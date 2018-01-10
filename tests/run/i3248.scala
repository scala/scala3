object Test extends App {
  class Foo(val name: String, val children: Int *)
  object Foo {
    def unapply(f: Foo) = Some((f.name, f.children))
  }

  def foo(f: Foo) = (f: Any) match {
    case Foo(name, ns: _*) => ns.length
    case List(ns: _*) => ns.length
  }

  case class Bar(val children: Int*)

  def bar(f: Any) = f match {
    case Bar(1, 2, 3) => 0
    case Bar(a, b) => a + b
    case Bar(ns: _*) => ns.length
  }

  assert(bar(new Bar(1, 2, 3)) == 0)
  assert(bar(new Bar(3, 2, 1)) == 3)
  assert(foo(new Foo("name", 1, 2, 3)) == 3)
}
