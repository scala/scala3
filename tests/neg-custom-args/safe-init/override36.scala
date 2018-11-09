import scala.annotation.init

class Foo {
  @init def h: Int = 20

  final def f: Int = h

  @init
  final def init: Int = f
}

class Bar extends Foo {
  private var m = 10
  override def h: Int = m
}

class Qux extends Bar {
  init                     // error
  override def h = a       // error
  private val a = 30
}