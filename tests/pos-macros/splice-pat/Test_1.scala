object Test {
  assert(Macro.macr(Macro.foo(1 + 2)) == (3, 1))
}
