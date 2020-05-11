import scala.quoted._

def h(using s: Scope)(m: s.Expr[M[String]]): s.Expr[Any] = g(m)