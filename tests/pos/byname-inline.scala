final class ByName1(val x: Int) extends AnyVal:
  inline def isEmpty: Boolean = x == 1
  inline def get: String = x.toString

object ByName1:
  inline def unapply(x: Int): ByName1 = new ByName1(x)

def useByName1PatMatch: String =
  val x = 1
  x match
    case ByName1(s) => s

final class ByName2(val x: Int) extends AnyVal:
  inline def isEmpty: false = false
  inline def get: Int = x

object ByName2:
  inline def unapply(x: Int): ByName2 = new ByName2(x)

def useByName2PatMatch: Int =
  val x = 1
  x match
    case ByName2(s) => s
