
import scala.quoted.*

object Macros {

  def impl(opt: Expr[Option[Int]]) (using Quotes): Expr[Int] = opt.valueOrError match {
    case Some(i) => Expr(i)
    case None => '{-1}
  }

  def impl2(opt: Expr[Option[Option[Int]]]) (using Quotes): Expr[Int] = impl(Expr(opt.valueOrError.flatten))

}
