class Foo {
  def f: Int = 50
  def init: Int = f
}

class Bar extends Foo {
  private var m = 10
  override def f: Int = m
}

class Qux extends Bar {
  init
  override def f = a
  private val a = 30    // warn
}