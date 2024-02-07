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

final class Accessor(val x: Int) extends AnyVal:
  inline def _1: Int = x + 1
  inline def _2: String = x.toString

final class ByName3(val x: Int) extends AnyVal:
  inline def isEmpty: Boolean = x == 1
  inline def get: Accessor = new Accessor(x)

object ByName3:
  inline def unapply(x: Int): ByName3 = new ByName3(x)

def useByName3PatMatch: (Int, String) =
  val x = 1
  x match
    case ByName3(i, s) => (i, s)
