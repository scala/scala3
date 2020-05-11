import scala.quoted._

object Test {
  def toExpr(using s: Scope)(x: Option[String]): s.Expr[String] = x match {
    case Some(s) =>
      's  // error
  }
}
