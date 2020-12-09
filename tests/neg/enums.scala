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
  val x1: Int => E4 = C4    // ok
  val x2: Int => E4 = C4(_) // ok
}

case class C4() extends E4 // error: cannot extend enum
case object O4 extends E4 // error: cannot extend enum

enum Captured[T] {
  case Case1[U](x: T) extends Captured[U] // error: illegal reference to type parameter T from enum case
  case Case2[U]()     extends Captured[T] // error: illegal reference to type parameter T from enum case
  case Case3          extends Captured[T] // error: illegal reference to type parameter T from enum case
}

enum Option[+T] derives CanEqual {
  case Some[T](x: T) extends Option[T]
  case None
}

object DollarNew {

  enum MyEnum:
    case A

  object MyEnum:

    def $new: MyEnum = new MyEnum with runtime.EnumValue { // error: anonymous class in method $new extends enum MyEnum, but extending enums is prohibited.
      override def $ordinal = 1
    }

    final val F = $new
}

object Test {

  class Unrelated

  val x: Option[Int] = Option.Some(1)
  x == new Unrelated // error: cannot compare

}
