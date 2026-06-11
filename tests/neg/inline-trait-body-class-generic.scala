inline trait A[T]:
  class Inner[U](u: U): // error
    val x: (T, U) = (???, u)
  def f: (T, String) = Inner("U").x

class B extends A[Int]