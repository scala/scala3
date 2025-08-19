class C {
  def m() = O.f2 // warn
}

object O {
  def foo(): Int = {
    val c = new C
    class D {
      def bar() = {
        c.m()
      }

      val f = bar()
    }
    val d = new D
    d.f
  }

  val f1 = foo()
  val f2: Int = 5
}