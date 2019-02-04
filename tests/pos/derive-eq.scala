
case class One() derives Eq
case class Two() derives Eq

implied for Eq[One, Two] = Eq

enum Lst[T] derives Eq {
  case Cons(x: T, xs: Lst[T])
  case Nil()
}

class Triple[S, T, U] derives Eq


object Test {
  implicitly[Eq[Lst[Lst[One]], Lst[Lst[Two]]]]
  implicitly[Eq[Triple[One, One, One],
                Triple[Two, Two, Two]]]

  val x: Triple[Lst[One], One, Two] = ???
  val y: Triple[Lst[Two], One, Two] = ???
  x == y
}
