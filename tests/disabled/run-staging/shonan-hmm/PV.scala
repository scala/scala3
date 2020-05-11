
import scala.quoted._

sealed trait PV[T]:
  val s: Scope

case class Sta[T](using val s: Scope)(x: T) extends PV[T]

case class Dyn[T](using val s: Scope)(x: s.Expr[T]) extends PV[T]

object Dyn:
  def apply[T](using s: Scope)(x: T)(using s.Liftable[T]): Dyn[T] = Dyn(Expr(x))

object Dyns {
  def dyn[T](using s: Scope)(pv: PV[T])(using s.Liftable[T]): s.Expr[T] = pv match {
    case Sta(x) => Expr(x)
    case Dyn(x) => x
  }
  def dyni(using s: Scope): PV[Int] => s.Expr[Int] = dyn[Int]
}
