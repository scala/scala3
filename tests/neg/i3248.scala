class Test {
  class Foo(val name: String, val children: Int *)
  object Foo {
    def unapplySeq(f: Foo) = Some((f.name, f.children))
  }

  def foo(f: Foo) = f match {
    case Foo(name, ns: _*) => 1 // error: not a valid unapply result type
  }
}
