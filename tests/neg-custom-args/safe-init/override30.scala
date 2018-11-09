import scala.annotation.init

class Foo {
  @init
  def f: Int = 50

  @init
  def g: Int = f
}

class Bar extends Foo {
  g
  f
}

class Qux extends Bar {
  val a = 30
  override def f = a       // error // error
}