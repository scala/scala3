
def test: Unit =
  summon[scala.util.NotGiven[T[Int]]] // error
