class A(a: Int) {
  class X {
    val x = a
  }

  trait Y {
    val y = a
  }

  trait Z(val o: A) extends o.Y {
    val z = a
  }

  class B extends X with Z(new A(4))
}