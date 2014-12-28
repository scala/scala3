object O {
  class A
  class B
  def f[T](x: T, y: T): T = x

  val x: A = f(new A { }, new A)

  val y: A | B = f(new A { }, new B)
}
