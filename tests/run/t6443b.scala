trait A {
  type D >: Null <: C|Null
  def foo(d: D)(d2: d.type): Unit
  trait C {
    def bar: Unit = foo(null)(null)
  }
}
object B extends A {
  class D1 extends C
  type D = D1|Null

  def foo(d: D)(d2: d.type): Unit = () // Bridge method required here!
}

object Test extends dotty.runtime.LegacyApp {
  new B.D1().bar
}
