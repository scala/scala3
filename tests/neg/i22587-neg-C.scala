// Negative Test Case C: Diverging Pattern in Match Type Case
// This SHOULD diverge because F[Int] matches the pattern F[Int], leading to F[F[Int]], which again matches F[Int].
// The match type definition cannot be applied because the pattern of its unique case diverges.

type F[A] = A match
  case F[Int] => F[F[Int]]

type Test = F[Int] // error