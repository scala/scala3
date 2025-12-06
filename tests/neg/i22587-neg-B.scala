// Negative Test Case 2: Wrapping Type Without Progress
// This SHOULD diverge because Wrap[Int] => Wrap[List[Int]] => Wrap[List[List[Int]]] => ...
// The type grows infinitely without reaching a base case

type Wrap[X] = X match
  case AnyVal => Wrap[List[X]]
  case AnyRef => Wrap[List[X]]

@main def test03(): Unit =
  val e1: Wrap[Int] = ??? // error
  println("Test 3 - Wrapping without progress:")
  println(s"e1 value: $e1")
