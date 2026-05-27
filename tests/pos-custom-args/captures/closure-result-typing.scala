import caps.*
class S extends SharedCapability
def test(c: S): Unit =
  val y: (x: S^{c}) -> Object^{fresh} = x => x
