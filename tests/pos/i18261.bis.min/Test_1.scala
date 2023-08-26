class Test:
  def test: Unit =
    summon[Cand[Int]]
    summon[Cand[Long]]
