// Positive Test Case A: Simple Recursive Deconstruction
// This should NOT diverge because recursion always reduces the type structure
// by unwrapping one layer (Option[t] -> t)

type DoubleUnwrap[X, Y] = (X, Y) match
  case (AnyVal, Y) => (X, Unwrap[Y])
  case (X, AnyVal) => (Unwrap[X], Y)
  case (X, Y) => (Unwrap[X], Unwrap[Y])

type Unwrap[X] = X match
  case Option[t] => Unwrap[t]
  case _ => X

@main def test01(): Unit =
  val e1: Unwrap[Option[Option[Int]]] = 42
  println("Test 1 - Simple recursive unwrapping:")
  println(s"e1 value: $e1")

  val e2: DoubleUnwrap[Option[String], Option[Int]] = ("hello", 42)
  println(s"e2 value: $e2")

  val e3: DoubleUnwrap[Int, Int] = (1, 2)
  println(s"e3 value: $e3")
