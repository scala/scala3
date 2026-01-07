class P[X, Y]

type Swap[X] = X match // error
  case P[x, y] => Swap[P[y, x]]

val z: P[String, Int] = ??? : Swap[P[Int, String]] // error
