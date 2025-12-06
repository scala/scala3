// Test Case 2: Direct Self-Reference Without Reduction (NEGATIVE)
// This SHOULD diverge because Loop[X] immediately expands to Loop[X]
// with no progress made - infinite loop without termination

type Loop[X] = X match
  case Int => Loop[Int]
  case _ => String

@main def test02(): Unit =
  val e1: Loop[Int] = ??? // error
  println("Test 2 - Direct self-reference:")
  println(s"e1 value: $e1")
