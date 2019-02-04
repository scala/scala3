
case class One() derives Eq
case class Two() derives Eq

implied for Eq[One, Two] = Eq

enum Lst[T] derives Eq {
  case Cons(x: T, xs: Lst[T])
  case Nil()
}

case class Triple[S, T, U] derives Eq


object Test extends App {
  Test1
  implicitly[Eq[Lst[Lst[One]], Lst[Lst[Two]]]]
  implicitly[Eq[Triple[One, One, One],
                Triple[Two, Two, Two]]]

  val x: Triple[List[One], One, Two] = ???
  val y: Triple[List[Two], One, Two] = ???
  val z: Triple[One, List[Two], One] = ???
  x == y // OK
  x == x // OK
  y == y // OK

  y == x // error
  x == z // error
  z == y // error
}
