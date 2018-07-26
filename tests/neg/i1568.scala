object Test {
  transparent def foo(n: Int) = foo(n) // error: cyclic reference
}
