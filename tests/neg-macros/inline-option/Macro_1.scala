
import scala.quoted.*

object Macro {
  def impl(opt: Expr[Option[Int]]) (using Quotes): Expr[Int] = opt.valueOrAbort match {
    case Some(i) => Expr(i)
    case None => '{-1}
  }
}