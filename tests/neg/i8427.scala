@SerialVersionUID(1L) // error
trait T

object Test {
  var t: T = compiletime.uninitialized
  def main(args: Array[String]) = println("hi")
}
