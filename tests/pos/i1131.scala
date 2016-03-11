class Outer {
  class Inner
}

trait MustBeATrait {
  val o = new Outer
  val inner = {
    class C {
      new o.Inner
    }
    new C
  }
  val inner2 = new o.Inner {}
}
