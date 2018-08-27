object Foo {
  rewrite def bar = new Bar // error
  class Bar private[Foo]()
}
