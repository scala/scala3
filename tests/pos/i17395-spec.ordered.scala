trait ThingWithPart { type Part }
type   PartField[A] = ThingWithPart { type Part = A }
type ExtractPart[B] = B match { case PartField[a] => a }

trait TC[C]
object TC:
  def tcForOptionPart[D](implicit tc: TC[ExtractPart[D]]): TC[Option[ExtractPart[D]]] = new {}

class Value
object Value:
  implicit val tcValue: TC[Value] = new {}

class ValuePartHolder extends ThingWithPart { type Part = Value }

class Test:
  def t1: Unit =
    val tc = TC.tcForOptionPart[ValuePartHolder]
