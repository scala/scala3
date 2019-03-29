package enums

enum List[+T] {
  case Cons[T](x: T, xs: List[T]) // error: missing extends
  case Snoc[U](xs: List[U], x: U) // error: missing extends
}

enum E1[T] {
  case C // error: cannot determine type argument
}

enum E2[+T, +U >: T] {
  case C // error: cannot determine type argument
}

enum E3[-T <: Ordered[T]] {
  case C // error: cannot determine type argument
}

enum E4 {
  case C
  case C4(x: Int)
}
object E4 {
  val x1: Int => E4 = C4    // error: found: C4, required: Int => E4
  val x2: Int => E4 = C4(_) // ok
}

case class C4() extends E4 // error: cannot extend enum
case object O4 extends E4 // error: cannot extend enum

enum Captured[T] {
  case Case1[U](x: T) extends Captured[U] // error: illegal reference to type parameter T from enum case
  case Case2[U]()     extends Captured[T] // error: illegal reference to type parameter T from enum case
  case Case3          extends Captured[T] // error: illegal reference to type parameter T from enum case
}

enum Option[+T] derives Eql {
  case Some(x: T)
  case None
}

object Test {

  class Unrelated

  val x: Option[Int] = Option.Some(1)
  x == new Unrelated // error: cannot compare

}

