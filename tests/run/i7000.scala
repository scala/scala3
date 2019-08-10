import scala.deriving._
import compiletime._

object Test extends App {
  case class B(v: Double) extends AnyVal
  val m0 = the[Mirror.ProductOf[B]]

  val v0 = m0.fromProduct(Tuple1(23.0))
  assert(v0 == B(23.0))

  case class C[T](v: T) extends AnyVal
  val m1 = the[Mirror.ProductOf[C[Double]]]

  val v1 = m1.fromProduct(Tuple1(23.0))
  assert(v1 == C(23.0))
}
