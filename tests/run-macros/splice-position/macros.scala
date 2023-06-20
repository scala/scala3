import scala.quoted.{Quotes, Expr, quotes}

inline def assertTrue(cond: Boolean) =
  ${ assertTrueImpl('cond) }

def assertTrueImpl(cond: Expr[Boolean])(using Quotes) =
  '{ if (!$cond) throw new Error(${'{""}}) }
