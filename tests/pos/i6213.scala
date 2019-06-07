object Test {
  class C { type T }
  inline def foo[U] <: Any = (??? : C { type T = U })

  foo[Int]
}