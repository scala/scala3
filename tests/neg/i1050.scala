trait A { type L <: Nothing }
trait B { type L >: Any}
object Test {
  lazy val x: A & B = ???
  val y: x.L = 1
  val z: String = y
}
object Test50 {
  trait A {
    type X = String
  }
  trait B {
    type X = Int
  }
  lazy val o: A & B = ???

  def xToString(x: o.X): String = x

  def intToString(i: Int): String = xToString(i)

  def main(args: Array[String]) = {
    val s: String = intToString(1)
  }
}
