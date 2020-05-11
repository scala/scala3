import scala.quoted.{ Scope, Expr }

def h(using s: Scope)(m: s.Expr[M]): s.Expr[Any] = g(m)