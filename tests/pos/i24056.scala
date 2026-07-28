trait DFBit
final class DFVal[+T, +M](val value: Int) extends AnyVal
type DFValOf[+T] = DFVal[T, Any]

class Top:
  val dmn1 = new scala.reflect.Selectable:
    val o: DFValOf[DFBit] = ???
  val x = dmn1.o
