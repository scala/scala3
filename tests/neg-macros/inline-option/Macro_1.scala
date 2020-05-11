
import scala.quoted._

object Macro {
  def impl(using s: Scope)(opt: s.Expr[Option[Int]]): s.Expr[Int] = opt.unliftOrError match {
    case Some(i) => Expr(i)
    case None => '{-1}
  }
}