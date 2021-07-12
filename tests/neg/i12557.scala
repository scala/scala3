package example

abstract class X[P <: Product](using val m: scala.deriving.Mirror.ProductOf[P]) {
  def unapply(p: P): m.MirroredElemTypes = ???
}

case class A(a: Int)
object A extends X[A]  // error

object Main {
  def main(args: Array[String]): Unit = {
    A.unapply(A(2))
  }
}
