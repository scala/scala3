import scala.quoted.*

def f(using Quotes)(x: Expr[Any]) = x match {
  case '{ val a: Int = 3; $y(identity(a)) } => // error: Exprected an identifier
  case '{ identity($y()) } => // error: Missing arguments for open pattern
}
