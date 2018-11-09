import scala.annotation.init

class Foo {
  def f: Int = 50

  @init
  def init: Int = f
}

class Bar extends Foo {
  private var m = 10
  override def f: Int = m
  init
}

class Qux extends Bar {
  val a = 30
  override def f = a       // error // error
}