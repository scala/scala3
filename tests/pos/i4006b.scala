class Foo {
  inline def foo: Int = try { 1 } catch { case _ => 4 } finally println("Hello")
  foo
}
