
import scala.quoted._

object Macros {

  def impl(using s: Scope)(opt: s.Expr[Option[Int]]): s.Expr[Int] = opt.unliftOrError match {
    case Some(i) => Expr(i)
    case None => '{-1}
  }

  def impl2(using s: Scope)(opt: s.Expr[Option[Option[Int]]]): s.Expr[Int] = impl(Expr(opt.unliftOrError.flatten))

}
