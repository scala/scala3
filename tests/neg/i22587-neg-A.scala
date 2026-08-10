// Negative Test Case A: Direct Self-Reference Without Reduction
// This SHOULD diverge because Loop[Int] => Loop[Float] => Loop[Double] => Loop[String] => Loop[Int] => ...
// The type cycles without reaching a base case.

type Loop[X] = X match
  case Int => Loop[Float]
  case Float => Loop[Double]
  case Double => Loop[String]
  case String => Loop[Int]
  case _ => String

type Test = Loop[Int] // error
