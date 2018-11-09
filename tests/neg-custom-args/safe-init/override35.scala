import scala.annotation.init

class Foo {
  @init
  def f: Int = 50

  @init
  def g: Int = f
}

trait Bar {
  def g: Int
  val a = g
}

class Qux extends Foo with Bar { // error
  private val x = 30
  override def f = x       // error
}