import scala.quoted._

inline def showList: String =
  ${ showListExpr }

private def showListExpr[T: Type](using Quotes): Expr[String] =
  Expr(Type.show[List])
