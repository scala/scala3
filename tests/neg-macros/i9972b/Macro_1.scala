
def test: Unit =
  summon[scala.util.Not[T[Int]]] // error
