object TestMacro {
  inline def test[T](inline t: T): T = ${ identity('{ identity(${ identity('{ identity(${ identity('t) }) }) }) }) } // error
}

object Test {
  TestMacro.test("x")
}
