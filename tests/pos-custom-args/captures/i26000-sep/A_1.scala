package p
import language.experimental.captureChecking
import caps.*

class File
class Box[A](val value: A)

object O:
  val l: List[[C^] -> (f: File^{C}) -> File^{f}] = List([C^] => (f: File^{C}) => f)
  val b: Box[[C^] -> (f: File^{C}) -> File^{f}] = Box([C^] => (f: File^{C}) => f)
