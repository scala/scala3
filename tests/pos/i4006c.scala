class Foo {
  rewrite def foo: Int = try { 1 } catch { case _ => 4 }
  foo
}
