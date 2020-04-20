object Test {
  class C { type T }
  transparent inline def foo[U]: Any = (??? : C { type T = U })

  foo[Int]
}