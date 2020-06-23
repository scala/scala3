
type Empty[X] = EmptyTuple
type Twice[X] = (X, X)

def test =
  val a1: EmptyTuple = ??? : Tuple.Fold[EmptyTuple, Nothing, Tuple2]
  val a2: (Int, (String, Nothing)) = ??? : Tuple.Fold[(Int, String), Nothing, Tuple2]
  val a3: Int | String | Char = ??? : Tuple.Fold[(Int, String, Char), Nothing, [X, Y] =>> X | Y]
