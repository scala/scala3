import quoted.*

inline def foo(s: Singleton): Unit = ${ fooImpl('s) }
def fooImpl(s: Expr[Singleton])(using Quotes) = '{}
