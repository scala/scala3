
import scala.quoted._

object Macros {

  def impl(opt: Option[Int]): Expr[Int] = opt match {
    case Some(i) => i.toExpr
    case None => '{-1}
  }

  def impl2(opt: Option[Option[Int]]): Expr[Int] = impl(opt.flatten)

}
