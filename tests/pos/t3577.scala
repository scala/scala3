case class Check[A](val value: A)

case class C2(checks: Check[_]*);

object C {
  def m(x : C2): Any = (null: Any) match {
    case C2(_, rest*) => {
      rest.map(_.value)
    }
  }
}

