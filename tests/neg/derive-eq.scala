
case class One() derives Eql
case class Two() derives Eql

implied for Eql[One, Two] = Eql.derived

enum Lst[T] derives Eql {
  case Cons(x: T, xs: Lst[T])
  case Nil()
}

case class Triple[S, T, U] derives Eql


object Test extends App {
  implicitly[Eql[Lst[Lst[One]], Lst[Lst[Two]]]]
  implicitly[Eql[Triple[One, One, One],
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
