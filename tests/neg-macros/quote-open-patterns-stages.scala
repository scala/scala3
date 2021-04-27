import scala.quoted.*

def f(using Quotes)(x: Expr[Any]) = x match {
  case '{ identity($y(x)) } => // error: access to value x from wrong staging level
}
