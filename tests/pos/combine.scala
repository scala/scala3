trait Semigroup[A] {
  def (x: A) combine (y: A): A
}
delegate for Semigroup[Int] = ???
delegate [A, B] for Semigroup[(A, B)] given Semigroup[A], Semigroup[B] = ???
object Test extends App {
  ((1, 1)) combine ((2, 2)) // doesn't compile
  ((1, 1): (Int, Int)) combine (2, 2) // compiles
  //the error that compiler spat out was "value combine is not a member of ((Int, Int)) => (Int, Int)". what's
}