package i26000external

class File
class Box[A](val value: A)

object Lib:
  // Other compilation units would see the result of the lambda as pure
  val f = Box([C^] => (x: File^{C}) => x) // error
  def mk(io: File^) = Box((x: File^{io}) => x) // error
