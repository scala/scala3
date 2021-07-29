class Test:
  import ArbitraryDerivation.given
  private def test[A: Arbitrary]: Unit = {}
  test[Foo]