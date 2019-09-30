object TestUnion {

  class A
  class B extends A
  class C extends A
  class D extends A

  val b = true
  val x: B | C = if (b) new B else new C
  val y: B | C = x
}

object TestUnion2 {
  class A
  class B
  def f[T](x: T, y: T): T = x

  val x: A = f(new A { }, new A)

  val y1: A | B = f(new A { }, new B)
  val y2: A | B = f[A | B](new A { }, new B)

  val z: A | B = if (???) new A{} else new B

  val z1: A | B = z
  val z2: A | B = if (???) new A else new B
}
