import scala.quoted._

def f(using s: Scope)(x: s.Expr[Any]) = x match {
  case '{ identity($y(x)) } => // error: access to value x from wrong staging level
}
