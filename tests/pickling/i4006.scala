class Foo {
  rewrite def foo: Int = try { 1 } finally println("Hello")
  foo
}
