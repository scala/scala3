object Test {
  inline def foo(n: Int) = foo(n) // error: cyclic reference
}
