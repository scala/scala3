object A:
  def foo(x: Int): Int => Int =
    if x <= 0 then
      return (a: Int) => a + B.n // warn

    (a: Int) => a * a + x

object B:
  val n = A.foo(-10)(20)

