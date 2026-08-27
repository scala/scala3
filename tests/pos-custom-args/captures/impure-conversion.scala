import language.experimental.captureChecking
import caps.*

trait Ctx extends SharedCapability

class C[T]:
  def value(using Ctx): T = ???

object C:
  given [T](using ctx: Ctx): (Conversion[C[T], T]^{ctx}) = _.value

def test(using Ctx): Unit =
  val c = new C[Int]
  val n: Int = c