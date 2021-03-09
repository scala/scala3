inline def failme1() = compiletime.error("fail")
transparent inline def failme2() = compiletime.error("fail")

@main def Test: Unit = {
  assert(!compiletime.testing.typeChecks("failme1()"))
  assert(!compiletime.testing.typeChecks("failme2()"))
  assert(!compiletime.testing.typeChecks("a b c"))
  assert(!compiletime.testing.typeChecks("true: Int"))
}
