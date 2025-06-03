class Foo {
  def f: Int = 50

  def g: Int = f
}

class Bar extends Foo {
  g
  f
}

class Qux extends Bar {
  val a = 30             // warn
  override def f = a
}