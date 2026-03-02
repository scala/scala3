trait A[T]:
  def f: T

trait B[T: A]:
  println(summon[A[T]].f)

trait C[T: A] extends B[T]

given a1: A[Int]:
  def f = 1

class D extends C[Int]:
  given a2: A[Int]:
    def f = 2

@main def Test = D()

