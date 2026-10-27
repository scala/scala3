package i26000b

class File
class Box[A](val value: A)
object io extends File, caps.SharedCapability

object Lib:
  val f: Box[[C^] -> (x: File^{C}) -> File^{x}] = Box([C^] => (x: File^{C}) => x)
  // Other compilation units see the parameter types and bounds of these lambdas as written
  val g = Box((x: File^{io}) => 1)
  val h = Box([C^ >: {io}] => (x: File^{C}) => 1)
