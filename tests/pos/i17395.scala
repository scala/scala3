//> using options -source:3.3

trait TC[T]

object TC {
  def optionTCForPart[T](implicit tc: TC[ExtractPart[T]]): TC[Option[ExtractPart[T]]] = new TC[Option[ExtractPart[T]]] {}
}

type ExtractPart[T] = T match {
  case PartField[t] => t
}
type PartField[T] = Any { type Part = T }

class ValuePartHolder {
  type Part = Value
}

class Value
object Value {
  implicit val tcValue: TC[Value] = new {}
}

@main def main(): Unit = {
//  import Value.tcValue // explicit import works around the issue, but shouldn't be necessary
  val tc = TC.optionTCForPart[ValuePartHolder]
  println(tc)
}
