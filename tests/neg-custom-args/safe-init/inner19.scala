class A {
  @scala.annotation.warm
  object O {
    val x = 10
    @scala.annotation.cold
    class B {
      val y = n   // error
      @scala.annotation.init
      def f: Int = n   // error // error
    }

    case class C(a: Int)
  }

  val n = 20
}

class B extends A {
  println((new O.B).f)
  O.C(4) // ok
  override val n = 50
}