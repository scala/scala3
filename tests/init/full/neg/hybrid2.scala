trait A {
  def g: Int
}

class Y {

  class X {
    class B extends A {
      def g = n
    }

    val b = new B
  }

  val x = new X
  x.b.g               // error

  val n = 10
}
