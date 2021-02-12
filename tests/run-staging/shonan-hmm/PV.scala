
import scala.quoted.*

sealed trait PV[T]

case class Sta[T](x: T) extends PV[T]

case class Dyn[T](x: Expr[T]) extends PV[T]

object Dyn:
  def apply[T: ToExpr](x: T)(using Quotes): Dyn[T] = Dyn(Expr(x))

object Dyns {
  def dyn[T: ToExpr](pv: PV[T])(using Quotes): Expr[T] = pv match {
    case Sta(x) => Expr(x)
    case Dyn(x) => x
  }
  def dyni(using Quotes): PV[Int] => Expr[Int] = dyn[Int]
}
