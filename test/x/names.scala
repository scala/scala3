package test

object Foo {
  def foo = 1
}

object Bar {
  def foo = 2
}

object Test {

  import Foo.foo

  object Inner {

    import Bar._
//!!    println(foo)
  }

}
