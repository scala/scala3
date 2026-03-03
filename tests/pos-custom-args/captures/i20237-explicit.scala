import language.experimental.captureChecking

class Cap extends caps.SharedCapability:
  def use[T](body: Cap => T) = body(this)

class Box[T](body: Cap => T):
  def open(any: Cap) = any.use(body)

object Box:
  def make[T](body: Cap => T)(any: Cap): Box[T]^{body} = Box(x => body(x))

def main =
  val givenCap: Cap = new Cap
  val xx: Cap => Int = y => 1
  val box = Box.make[Int](xx)(givenCap).open