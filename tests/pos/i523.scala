class X
class A {
  def foo[T1, T2 >: T1]: Unit = {}

  def test = {
    foo[X, X]
  }
}
