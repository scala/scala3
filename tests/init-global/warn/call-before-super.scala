class C(i: Int = 42, j: Int = 27)

object X extends C(j = X.foo()): // warn
  def foo() = 5

@main def test = println:
  X
