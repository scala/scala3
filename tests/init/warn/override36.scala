class Foo {
  def h: Int = 20

  final def f: Int = h

  final def init: Int = f
}

class Bar extends Foo {
  private var m = 10
  override def h: Int = m
}

class Qux extends Bar {
  init
  override def h = a
  private val a = 30  // warn
}