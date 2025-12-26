// Negative Test Case 2: Wrapping Type Without Progress
// This SHOULD diverge because Wrap[Int] => Wrap[List[Int]] => Wrap[List[List[Int]]] => ...
// The type grows infinitely without reaching a base case

type Wrap[X] = X match
  case AnyVal => Wrap[List[X]]
  case AnyRef => Wrap[List[X]]

type Test = Wrap[Int] // error
