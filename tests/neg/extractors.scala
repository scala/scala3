object A {}

object B {

  def unapply[T](x: T): Option[x.type] = ???

}

object C {
  def unapply[T](x: T, y: T): Option[T] = ???
}

object D {
  def unapply[T](): Option[T] = ???
}

object Test {

  val x: Any = ???
  x match {
    case A(y) => ??? // error: A cannot be used as an extractor in a pattern it lacks a unapply or unapplySeq method
    case B(y) => ??? // error: B cannot be used as an extractor in a pattern its unapply method of type (x: T)Option[T(x)] has a dependent type
    case C(y) => ??? // error: C cannot be used as an extractor in a pattern its unapply method of type (x: T, y: T)Option[T] does not take a single parameter
    case D(y) => ??? // error: D cannot be used as an extractor in a pattern its unapply method of type ()Option[T] does not take a single parameter
  }

}
