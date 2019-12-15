class Foo {
  def f: Int = 50

  def g: Int = f
}

trait Bar {
  def g: Int
  val a = g
}

class Qux extends Foo with Bar {
  private val x = 30             // error
  override def f = x
}