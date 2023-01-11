// This tests that f1 is recognized as an irrefutable pattern and f2 is not
class A {
  case class Foo[T](x: T)

  def f1(xs: List[Foo[Int]]) = {
    for (Foo(x: Int) <- xs) yield x
  }
  def f2(xs: List[Foo[Any]]) = {
    for (case Foo(x: Int) <- xs) yield x
  }
}
