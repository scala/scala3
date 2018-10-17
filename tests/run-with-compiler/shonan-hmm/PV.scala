
import scala.quoted._

sealed trait PV[T]

case class Sta[T](x: T) extends PV[T]

case class Dyn[T](x: Expr[T]) extends PV[T]

object Dyns {
  def dyn[T: Liftable](pv: PV[T]): Staged[T] = pv match {
    case Sta(x) => x.toExpr
    case Dyn(x) => x
  }
  val dyni: PV[Int] => Staged[Int] = dyn[Int]
}
