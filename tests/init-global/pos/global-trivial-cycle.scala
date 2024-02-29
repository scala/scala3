object C:
  val n: Int = A.n

object A:
  val x: Int = 10
  val n: Int  = B.foo()

object B:
  def foo(): Int = A.x
