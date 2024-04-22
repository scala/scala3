import language.experimental.captureChecking
import scala.annotation.capability

@capability class Cap:
  def use[T](body: Cap ?=> T) = body(using this)

class Box[T](body: Cap ?=> T):
  inline def open(using cap: Cap) = cap.use(body)

object Box:
  def make[T](body: Cap ?=> T)(using Cap): Box[T]^{body} = Box(body)

def main =
  given Cap = new Cap
  val box = Box.make(1).open