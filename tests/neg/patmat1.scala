trait A
trait B
class C extends A with B
case class D()
object X {
  def unapply(x: B): Boolean = false
}

object Test {
  def main(args: Array[String]) = {
    val ca: A = new C
    ca match {
      case x: B =>
      case X() =>
      case D() => // ok, but scalac disagrees
    }
    val cc = new C
    cc match {
      case x: B =>
      case X() =>
      case D() =>  // error: neither a subtype not a supertype
    }
  }
}
