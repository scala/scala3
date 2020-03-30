object Test {

  class A
  class B extends A
  class C extends A
  class D extends A

  val b = true
  val x = if (b) new B else new C
  val y: B | C = x  // error
}

object O {
  class A
  class B
  def f[T](x: T, y: T): T = x

  val x: A = f(new A { }, new A)

  val y1: A | B = f(new A { }, new B) // ok
  val y2: A | B = f[A | B](new A { }, new B) // ok

  val z = if (???) new A{} else new B

  val z1: A | B = z // error

  val z2: A | B = if (???) new A else new B // ok
}
