package p
import language.experimental.captureChecking
import caps.*

class File

// The body of a polymorphic closure is not a closure, so the parameter
// types of its function type stay inferred
object Test:
  def test(x: File^, y: File^, c: Boolean, g1: File^{x} -> Unit, g2: File^{y} -> Unit) =
    val f1 = [C^] => (f: File^{C}) => if c then g1 else g2
    val f2 = [C^] => (f: File^{C}) => if c then g2 else g1
    val f3 = [T] => (u: T) => if c then g1 else g2
