package ord

trait Ord[A]

object Ord {
  given [A] => (A => java.lang.Comparable[? >: A]) => Ord[A] as ordered = ???
}

def sort[A: Ord](as: List[A]): List[A] = ???

class Foo

val sortedFoos = sort(List(new Foo)) // error
