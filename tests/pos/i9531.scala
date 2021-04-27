trait Scope:
  type Expr[+T]

def exprQuote[T](x: T)(using s: Scope, dummy: Null = null): s.Expr[T] = ???
def exprQuote[T <: Singleton](x: T)(using s: Scope): s.Expr[T] = ???

def test(using s: Scope): Unit =
  val t1: s.Expr[1] = exprQuote(1)
  val t2 = exprQuote(1)
  val t3: s.Expr[1] = t2
