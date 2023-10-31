class Foo

class Bar extends Foo
object Bar {
  implicit val listbar: List[Bar] = ???
}

class Test {
  def get1(implicit lf: List[? <: Bar]) = {}
  def get2(implicit lf: List[? >: Bar]) = {}

  get1
  get2
}
