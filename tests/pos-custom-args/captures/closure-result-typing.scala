import caps.fresh
def test(c: Object^): Unit =
  val y: (x: Object^{c}) -> Object^{fresh} = x => x
