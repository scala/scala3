// Test Case 4: Two-Parameter Match Type with Swapping (POSITIVE)
// This should NOT diverge because even though parameters swap positions,
// the tuple structure reduces (Tuple2 -> single type)

type Swap[X, Y] = (X, Y) match
  case (Int, String) => Y
  case (String, Int) => X
  case (List[a], b) => Swap[a, b]
  case (a, List[b]) => Swap[a, b]
  case _ => X

@main def test04(): Unit =
  val e1: Swap[Int, String] = "result"
  println("Test 4 - Two-parameter swap:")
  println(s"e1 value: $e1")
  
  val e2: Swap[String, Int] = "swapped"
  println(s"e2 value: $e2")
  
  val e3: Swap[List[List[Int]], List[String]] = "42"
  println(s"e3 value: $e3")
