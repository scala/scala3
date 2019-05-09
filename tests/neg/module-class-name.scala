object Test {
  class C
  object C

  def f[T](x: T): T = x

  val x: C = C    // error: Found:    Test.C.type

  val y: C = f(C) // error: Found:    Test.C.type

  def z = f(C)
  val z1: C = z    // error: Found:    object Test.C
  val z2: Int = z  // error: Found:    object Test.C

  C.foo  // error: value foo is not a member of object Test.C
}
