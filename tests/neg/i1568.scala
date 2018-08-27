object Test {
  rewrite def foo(n: Int) = foo(n) // error: cyclic reference
}
