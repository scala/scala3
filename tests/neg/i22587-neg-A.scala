// Negative Test Case A: Direct Self-Reference Without Reduction
// This SHOULD diverge because Loop[Int] => Loop[Float] => Loop[Double] => Loop[String] => Loop[Int] => ...
// The type cycles without reaching a base case.

type Loop[X] = X match
  case Int => Loop[Float]
  case Float => Loop[Double]
  case Double => Loop[String]
  case String => Loop[Int]
  case _ => String

@main def test02(): Unit =
  val e1: Loop[Int] = ??? // error
  println("Test 2 - Direct self-reference:")
  println(s"e1 value: $e1")
