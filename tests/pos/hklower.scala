class Test {

  type T[X]
  type U[X] = T[X]

  type V[X] >: T[X]
  type W[X] >: T[X] <: T[X]

  def f[C[X] >: T[X]](x: C[Int]) = ???

  val v: V[Int] = ???
  val t: T[Int] = ???

  f[V](v)

  f[V](t)


}
class Test2 {

  class T[X]
  type U[X] = T[X]

  type V[X] >: T[X]
  type W[X] >: T[X] <: T[X]

  def f[C[X] >: T[X]](x: C[Int]) = ???

  val v: V[Int] = ???
  val t: T[Int] = ???

  f[V](v)

  f[V](t)

  var x: V[Int] = compiletime.uninitialized
  x = t


}
