import language.experimental.captureChecking
import caps.*

class Cap extends caps.Capability:
  def use[T](body: Cap ?=> T) = body(using this)

class Cap2 extends caps.Capability:
  def use[T](body: Cap2 => T) = body(this)

class Box[T](body: Cap ?=> T):
  inline def open(using cap: Cap) = cap.use(body)

object Box:
  def make[T](body: Cap ?=> T)(using Cap): Box[T]^{body} = Box(body)

def main =
  given Cap = new Cap
  val box = Box.make(1).open