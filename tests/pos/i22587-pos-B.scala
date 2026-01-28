// Positive Test Case B: Binary Recursive construction
// This should NOT diverge because although types have same complexity, the structure
// changes by incrementing by one the numeric parameter until reaching a base case.


type Increase[X, Y, Z] = (X, Y, Z) match
  case (X, Y, 0) => Increase[X, Y, 1]
  case (X, 0, _) => Increase[X, 1, 0]
  case (0, _, _) => Increase[1, 0, 0]
  case _ => "done"

@main def test04(): Unit =
  val e1: Increase[0, 0, 0] = "done"
