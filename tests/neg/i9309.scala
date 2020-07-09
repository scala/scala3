case class Foo(foo: Int) {
  inline def foo = (foo) // error
}