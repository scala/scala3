object Foo {
  inline def bar = new Bar // error
  class Bar private[Foo]()
}
