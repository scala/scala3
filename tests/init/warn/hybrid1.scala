trait A {
  def g: Int
}

class Y {
  class X {
    class B extends A {
      def g = n
    }
  }

  val x = new X

  class C extends x.B {
    g
  }

  new C

  val n = 10   // warn
}
