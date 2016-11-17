trait Foo {
  def test (x : test) : Int // error: not found test
  def f(x : src) : Int // error
  def g(x : src.project) : Int // error
}
