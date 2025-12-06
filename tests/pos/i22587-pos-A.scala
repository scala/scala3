// Test Case 1: Simple Recursive Deconstruction (POSITIVE)
// This should NOT diverge because recursion always reduces the type structure
// by unwrapping one layer (Option[t] -> t)

type Unwrap[X] = X match
  case Option[t] => Unwrap[t]
  case _ => X

@main def test01(): Unit =
  val e1: Unwrap[Option[Option[Int]]] = 42
  println("Test 1 - Simple recursive unwrapping:")
  println(s"e1 value: $e1")
  
  val e2: Unwrap[Option[String]] = "hello"
  println(s"e2 value: $e2")
  
  val e3: Unwrap[Int] = 99
  println(s"e3 value: $e3")
