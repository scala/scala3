object Bug {
  class C[T] { type TT = T }
  def foo[U](x: Int)(y: C[U])(z: y.TT): Unit = {}
  foo(3)(new C[String])("hello")
}

object Bug2 {
  class C { type T }
  class D extends C { type T = String }
  def foo(x: Int)(y: C)(z: y.T): Unit = {}
  foo(3)(new D)("hello")
}
