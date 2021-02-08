class Test:
  def test: Unit =
    assert2(scala.compiletime.testing.typeChecks(stripMargin("|1 + 1")))

  inline def stripMargin(inline x: String): String = x

  transparent inline def assert2(inline assertion: Boolean): Unit =
    if !assertion then scala.runtime.Scala3RunTime.assertFailed()
