package test

object Foo {
  def foo = 1
}

object Bar {
  def foo = 2
}

object Test123 {

  import Foo.foo

  object Inner {

    import Bar._
//!!    println(foo)
  }

}
