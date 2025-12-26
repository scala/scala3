// Negative Test Case D: Match Type with Nested Self-Application
// This SHOULD diverge because F[F[Int]] => F[F[F[Int]]] => F[F[F[F[Int]]]] => ...
// The motivation of this test is that the memory of arguments (stack) contains F[Int] since the beginning

type F[A] = A match
  case Int => F[F[A]]
  case _ => F[F[A]]

type Test = F[F[Int]] // error