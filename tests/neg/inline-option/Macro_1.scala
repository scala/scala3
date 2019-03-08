
import scala.quoted._

object Macro {
  def impl(opt: Option[Int]): Expr[Int] = opt match {
    case Some(i) => i.toExpr
    case None => '{-1}
  }
}