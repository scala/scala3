object FooMacros {
  def foo[T]: String = macro Foo.fooImpl[T] // error: Scala 2 macro definition needs to be enabled
}
