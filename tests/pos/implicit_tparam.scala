class Foo[T]
class Bar extends Foo[A]

class A
object A {
  implicit val bar: Bar = new Bar
}

object Test {
  def getBar(implicit bar: Bar) = bar
  getBar
}
