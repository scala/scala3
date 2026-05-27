trait DFBit
final class DFVal[+T, +M](val value: Int) extends AnyVal
type DFValOf[+T] = DFVal[T, Any]

class Top:
  val dmn1 = new scala.reflect.Selectable:
    val o: DFValOf[DFBit] = DFVal(42)
  val x: DFValOf[DFBit] = dmn1.o

object Test:
  def main(args: Array[String]): Unit =
    val t = Top()
    assert(t.x.value == 42)
