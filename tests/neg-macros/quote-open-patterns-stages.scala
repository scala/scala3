import scala.quoted._

def f(using QuoteContext)(x: Expr[Any]) = x match {
  case '{ identity($y(x)) } => // error: access to value x from wrong staging level
}
