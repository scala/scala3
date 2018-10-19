object Model {
  sealed trait Field[T]
  case object StringField extends Field[String]
  case object BoolField extends Field[Boolean]

  sealed trait Value[T]
  case class Literal(v: String) extends Value[String]
  case class Bool(v: Boolean) extends Value[Boolean]

  sealed trait Expression[T]
  case class Equality[T](field: Field[T], value: Value[T]) extends Expression[T]

  def interpret[T](expr: Expression[T]): Int = expr match {
    case Equality(StringField, Literal(v)) => 1
    case Equality(BoolField, Bool(v)) => 2
  }

  // T - T(s1, s2, ...)
  // if T contains type parameter, get its dimension types.
  //
  // actively decompose its dimensions, and then do subtype checking to
  // see if the type parameter can be instantiated.
}
