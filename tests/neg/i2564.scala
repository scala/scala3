object Foo {
  transparent def bar = new Bar // error
  class Bar private[Foo]()
}
