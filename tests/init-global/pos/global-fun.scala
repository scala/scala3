class C:
  def double(x: Int): Int = x * 2

object A:
  val n: Int = 10
  val f: C => Int  = foo(n)

  def foo(x: Int): C => Int =
    c => c.double(x)


object B:
  var y = A.f(new C)
