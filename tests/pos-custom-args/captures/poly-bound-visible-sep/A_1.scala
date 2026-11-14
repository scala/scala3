package p
import language.experimental.captureChecking
import caps.*

class File
class Box[A](val value: A)

// The bound `{any}` of `C` is pickled, so it is in the externally visible types
object O:
  val pg = Box([C^] => (x: File^{C}) => 1)
  def mk() = Box([C^] => (f: File^{C}) => f)
  inline def mkInline() = Box([C^] => (f: File^{C}) => f)
