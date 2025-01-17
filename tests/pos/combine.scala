trait Semigroup[A] {
  extension (x: A) def combine(y: A): A
}
given Semigroup[Int] = ???
given [A, B] => (Semigroup[A], Semigroup[B]) => Semigroup[(A, B)]  = ???
object Test extends App {
  ((1, 1)) combine ((2, 2)) // doesn't compile
  ((1, 1): (Int, Int)) combine (2, 2) // compiles
  //the error that compiler spat out was "value combine is not a member of ((Int, Int)) => (Int, Int)". what's
}