class Outer {
  def foo = {
    class C {
      val x = n
    }
    class D {
      new C
    }

    new D
  }

  foo

  val n = 10 // warn
}
