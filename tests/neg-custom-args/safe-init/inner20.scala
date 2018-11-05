class A {
  class O {
    val x = 10
    @scala.annotation.init
    class B {
      val y = n
      def f: Int = n
    }
  }

  val n = 20
}

class B extends A {
  val o = new O               // error
  println((new o.B).f)
  override val n = 50
}