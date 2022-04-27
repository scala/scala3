import scala.quoted.*

inline def switch[T](n: Int)(inline xs: T*): T = ${ switchExpr('n, 'xs) }

private def switchExpr[T: Type](n: Expr[Int], casesExpr: Expr[Seq[T]])(using Quotes): Expr[T] =
  casesExpr match
    case Varargs(caseExprs) => switchExpr(n, caseExprs)
    case _ => quotes.reflect.report.errorAndAbort("switch does not support varargs unpacking")

private def switchExpr[T: Type](n: Expr[Int], caseExprs: Seq[Expr[T]])(using Quotes): Expr[T] =
  import quotes.reflect.*
  val cases =
    for (caseExpr, i) <- caseExprs.zipWithIndex
    yield CaseDef(Literal(IntConstant(i)), None, caseExpr.asTerm)
  Match(n.asTerm, cases.toList).asExprOf[T]
