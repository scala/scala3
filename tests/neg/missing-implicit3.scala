package ord

trait Ord[A]

object Ord {
  given ordered[A](using A => java.lang.Comparable[? >: A]): Ord[A] = ???
}

def sort[A: Ord](as: List[A]): List[A] = ???

class Foo

val sortedFoos = sort(List(new Foo)) // error
