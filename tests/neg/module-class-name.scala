object Test {
  class C
  object C

  def f[T](x: T): T = x

  val x: C = C    // error

  val y: C = f(C) // error

  def z = f(C)
  val z1: C = z    // error
  val z2: Int = z  // error

  C.foo  // error: value foo is not a member of object Test.C
}
