import scala.quoted.{_, given}

object Test {
  def toExpr(x: Option[String]): Expr[String] = x match {
    case Some(s) =>
      's  // error
  }
}
