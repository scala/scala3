trait TC[T]

object TC {
  def optionTCForPart[T](implicit tc: TC[ExtractPart[T]]): TC[Option[ExtractPart[T]]] = new TC[Option[ExtractPart[T]]] {}
}

trait ThingWithPart {
  type Part
}

type ExtractPart[T] = T match {
  case PartField[t] => t
}
type PartField[T] = ThingWithPart { type Part = T }

class ValuePartHolder extends ThingWithPart {
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
