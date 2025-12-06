// Test Case 5: Two-Parameter Match Type with Swapping (Negative)
// This should diverge because even though recursion should reduce,
// the sum of parameters size does not decrease in one recursive step.

type Swap[X, Y] = (X, Y) match
  case (Int, String) => Swap[Y, X]
  case (String, Int) => X
  case (List[a], b) => Swap[a, b]
  case (a, List[b]) => Swap[a, b]
  case _ => X

@main def test05(): Unit =
  val e: Swap[List[List[Int]], List[String]] = ??? // error
  println(s"e value: $e")