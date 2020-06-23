
type Empty[X] = EmptyTuple
type Twice[X] = (X, X)

def test =
  val a1: EmptyTuple = ??? : Tuple.FlatMap[EmptyTuple, Empty]
  val a2: EmptyTuple = ??? : Tuple.FlatMap[(Int, String), Empty]

  val b1: EmptyTuple = ??? : Tuple.FlatMap[EmptyTuple, Tuple1]
  val b2: (Int, String) = ??? : Tuple.FlatMap[(Int, String), Tuple1]

  val c1: EmptyTuple = ??? : Tuple.FlatMap[EmptyTuple, Twice]
  val c2: (Int, Int, String, String) = ??? : Tuple.FlatMap[(Int, String), Twice]
  val c3: (Int, List[Int], String, List[String]) = ??? : Tuple.FlatMap[(Int, String), [X] =>> (X, List[X])]
