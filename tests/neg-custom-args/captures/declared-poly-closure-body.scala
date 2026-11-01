package p
import language.experimental.captureChecking
import caps.*

class File

// The parameter types of a polymorphic function returned by the body of a
// polymorphic closure stay inferred, but its bounds are kept
object Test:
  def test(a: File^): File =
    val f = [C^] => (u: Unit) => { println(); [D^ >: {a}] => (v: Unit) => (a: File^{D}) }
    f[{}](())[{}](()) // error
