trait SomeRestriction

enum ADT {
  case A extends ADT
  case B extends ADT with SomeRestriction
}

object MinimalExample {
  val b: ADT & SomeRestriction = ADT.B

  b match {
    case ADT.B => ???
  }
}
