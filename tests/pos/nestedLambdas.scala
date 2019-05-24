class Test {

  type T = [X] =>> [Y] =>> (X, Y)

  type A[X] = [Y] =>> (X, Y)

  type B[X] = (X, X)

  val x: T[Int][Boolean] = ???

  val y: A[Int][Boolean] = x

  def f[X <: T[Int]] = ???

  f[A[Int]]

  def g[X <: T] = ???

  g[A]

}
