class Foo

class Bar extends Foo
object Bar {
  implicit val listbar: List[Bar] = ???
}

class Test {
  def get1(implicit lf: List[_ <: Bar]) = {}
  def get2(implicit lf: List[_ >: Bar]) = {}

  get1 // works
  get2 // error
}
