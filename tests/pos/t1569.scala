// See pos/t1569a.scala for related examples that work.
object Bug {
  class C { type T }
  def foo(x: Int)(y: C)(z: y.T): Unit = {}
  foo(3)(new C { type T = String })("hello")
}
