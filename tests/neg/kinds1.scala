object Test {

  class C[T]
  class C2[T[X]]

  class B

  val x: C[C] = ??? // error: Type argument does not have the same kind as its bound
  val y: C2[C] = ???

  def f[T] = ???

  def f2[T[X]] = ???

  f2[C]
}
