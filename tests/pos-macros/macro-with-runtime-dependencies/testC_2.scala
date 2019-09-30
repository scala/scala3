object TestC {
  inline def f() = ${ TestB.testB() }

  f()
}
