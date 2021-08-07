
case class One() derives CanEqual
case class Two() derives CanEqual

given CanEqual[One, Two] = CanEqual.derived

enum Lst[T] derives CanEqual {
  case Cons(x: T, xs: Lst[T])
  case Nil()
}

case class Triple[S, T, U]() derives CanEqual

object Test extends App {
  implicitly[CanEqual[Lst[Lst[One]], Lst[Lst[Two]]]]
  implicitly[CanEqual[Triple[One, One, One],
                      Triple[Two, Two, Two]]]

  val x: Triple[List[One], One, Two] = ???
  val y: Triple[List[Two], One, Two] = ???
  val z: Triple[One, List[Two], One] = ???
  x == y // OK
  y == x // OK
  x == x // OK
  y == y // OK

  x == z // error
  z == y // error
}
