object Foo {
  val foo = Nil
  object foo // error
  foo(foo: _*) // error
}
