class Outer {
  def foo = {
    val y = 5
    class C {
      val x = y
    }
    class D {
      new C
    }

    new D
  }

  foo

  val n = 10 // warn
}

object O {
  val c = new Outer
  val d: Object = c.foo
}