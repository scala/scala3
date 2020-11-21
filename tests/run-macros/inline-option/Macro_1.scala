
import scala.quoted._

object Macros {

  def impl(opt: Expr[Option[Int]]) (using Quotes): Expr[Int] = opt.unliftOrError match {
    case Some(i) => Expr(i)
    case None => '{-1}
  }

  def impl2(opt: Expr[Option[Option[Int]]]) (using Quotes): Expr[Int] = impl(Expr(opt.unliftOrError.flatten))

}
