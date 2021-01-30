import scala.quoted.*

inline def seq = ${fooImpl}

def fooImpl(using Quotes) = {
  Expr.ofSeq(List('{1}, '{2}, '{3}))
}
