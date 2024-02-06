final class ByName1(val x: Int) extends AnyVal:
  inline def isEmpty: Boolean = false
  inline def get: String = x.toString

object ByName1:
  inline def unapply(x: Int): ByName1 = new ByName1(x)

def useByNamePatMatch: String =
  val x = 1
  x match
    case ByName1(s) => s
