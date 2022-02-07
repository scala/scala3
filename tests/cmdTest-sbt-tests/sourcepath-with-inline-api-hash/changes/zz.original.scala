package a

object Foo: // note that `Foo` is defined in `zz.scala`
  class Local
  inline def foo(using Local): Nothing =
    ???
