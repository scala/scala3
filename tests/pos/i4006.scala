class Foo {
  transparent inline def foo: Int = try { 1 } finally println("Hello")
  foo
}
