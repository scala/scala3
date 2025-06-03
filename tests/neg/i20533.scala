def mapF(h: [X, Y] => (X, Y) => Map[X, Y]): Unit = ???

def test =
  mapF(
    [X] => (x, y) => Map(x -> y) // error
  )
