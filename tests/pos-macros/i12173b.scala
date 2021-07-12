object TestMacro {
  inline def test[T](inline t: T): T = ${ '{ ${ '{ ${ 't } } } } }
}

object Test {
  TestMacro.test("x")
}
