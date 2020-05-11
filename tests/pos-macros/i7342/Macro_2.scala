import scala.quoted._

def h(using s: Scope)(m: s.Expr[Foo]): s.Expr[Any] = g(m)
