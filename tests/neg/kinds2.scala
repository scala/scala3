object Test {

  class C[T]
  class C2[T[X]]

  class B

  val y: C2[C] = ???

  def f[T] = ???

  def f2[T[X]] = ???

  f[C] // error: missing type parameter(s)
  f2[C]

}
