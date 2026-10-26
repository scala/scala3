package i26000b

class File
class Box[A](val value: A)

// Other compilation units see `f` with its inferred capture sets dropped,
// so the parameter types of its lambda stay inferred as well
object Lib:
  val f = Box([C^] => (x: File^{C}) => x)
