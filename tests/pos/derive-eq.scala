
case class One() derives Eql
case class Two() derives Eql

delegate for Eql[One, Two] = Eql.derived

enum Lst[T] derives Eql {
  case Cons(x: T, xs: Lst[T])
  case Nil()
}

class Triple[S, T, U] derives Eql


object Test {
  implicitly[Eql[Lst[Lst[One]], Lst[Lst[Two]]]]
  implicitly[Eql[Triple[One, One, One],
                Triple[Two, Two, Two]]]

  val x: Triple[Lst[One], One, Two] = ???
  val y: Triple[Lst[Two], One, Two] = ???
  x == y
}
