// This was t1569.scala.
// It fails in dotty because the expected type of the anonymous function in the last line
// is fully determined (C). So that type is taken as the type of the anonymous function.
// See pos/t1569a.scala for related examples that work.
object Bug {
  class C { type T }
  def foo(x: Int)(y: C)(z: y.T): Unit = {}
  foo(3)(new C { type T = String })("hello")          // error
}
