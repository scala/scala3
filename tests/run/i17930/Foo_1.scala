package eu.joaocosta.defaultstest

object Foo {
  def foo(x: Int, y: Int = 5): Int = x + y
}

object Bar {
  export Foo.*
}

object App {
  println(Bar.foo(2)) // Works
}
