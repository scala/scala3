import scala.quoted._

def myMacro(a: Int)(using Quotes) = Expr(a)
def baz = 42
inline def bar = baz
inline def foo = ${myMacro(bar)} // error
