import language.experimental.captureChecking

class Cap extends caps.Capability:
  def use[T](body: Cap => T) = body(this)

class Box[T](body: Cap => T):
  def open(cap: Cap) = cap.use(body)

object Box:
  def make[T](body: Cap => T)(cap: Cap): Box[T]^{body} = Box(x => body(x))

def main =
  val givenCap: Cap = new Cap
  val xx: Cap => Int = y => 1
  val box = Box.make[Int](xx)(givenCap).open