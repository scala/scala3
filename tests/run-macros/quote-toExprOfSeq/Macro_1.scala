import scala.quoted._

inline def seq = ${fooImpl}

def fooImpl(using s: Scope) = {
  Expr.ofSeq(List('{1}, '{2}, '{3}))
}
