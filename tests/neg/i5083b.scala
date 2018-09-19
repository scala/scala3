class A(a: Int) {
  class X {
    val x = a
  }

  trait Y {
    val y = a
  }

  val z: Z = ???

  trait Z(val o: A) extends z.o.Y   // error: cyclic reference `z`

  class B extends X with Z(new A(4))
}
