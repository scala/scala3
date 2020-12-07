
import scala.quoted._

object Macros {

  def impl(opt: Expr[Option[Int]]) (using Quotes): Expr[Int] = opt.valueOrError match {
    case Some(i) => Value(i)
    case None => '{-1}
  }

  def impl2(opt: Expr[Option[Option[Int]]]) (using Quotes): Expr[Int] = impl(Value(opt.valueOrError.flatten))

}
