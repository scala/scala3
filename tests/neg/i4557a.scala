class A[X]

object T {
  type Foo[X, Y] = [Z] =>> A[X]
  class B extends Foo[Int, Int] // error
}
