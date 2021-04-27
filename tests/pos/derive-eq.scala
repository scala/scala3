
case class One() derives CanEqual
case class Two() derives CanEqual

given CanEqual[One, Two] = CanEqual.derived

enum Lst[T] derives CanEqual {
  case Cons(x: T, xs: Lst[T])
  case Nil()
}

class Triple[S, T, U] derives CanEqual


object Test {
  implicitly[CanEqual[Lst[Lst[One]], Lst[Lst[Two]]]]
  implicitly[CanEqual[Triple[One, One, One],
                Triple[Two, Two, Two]]]

  val x: Triple[Lst[One], One, Two] = ???
  val y: Triple[Lst[Two], One, Two] = ???
  x == y
}
