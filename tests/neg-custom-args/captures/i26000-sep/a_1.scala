package i26000sep

import language.experimental.captureChecking
import caps.*

class File
class Box[A](val value: A)

// Other compilation units see `f` with its inferred capture sets dropped,
// so the parameter types of its lambda stay inferred as well
object Lib:
  val f = Box([C^] => (x: File^{C}) => x)
