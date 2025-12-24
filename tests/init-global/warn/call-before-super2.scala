class C(j: Int)

object A:
  def foo = X.k // warn

object X extends C(A.foo):
  def k = 5

@main def test = println:
  X