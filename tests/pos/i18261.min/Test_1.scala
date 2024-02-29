class Test:
  def test: Unit =
    summon[Foo[Int]]
    summon[Foo[Long]]
