package p
import language.experimental.captureChecking
import caps.*

class File

// The pickled type of `l` has (f: File) as parameter type, so it needs an explicit type
object O:
  val l = List([C^] => (f: File^{C}) => f) // error
