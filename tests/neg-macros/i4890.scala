import scala.quoted.*

object Test {
  def toExpr(x: Option[String]): Expr[String] = x match {
    case Some(s) =>
      's  // error
  }
}
