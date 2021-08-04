trait SomeRestriction

enum ADT {
  case A
  case B extends ADT with SomeRestriction
}

object MinimalExample {
  val b: ADT & SomeRestriction = ADT.B

  b match {
    case ADT.B => ???
  }
}
