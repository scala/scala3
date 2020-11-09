import scala.quoted._

class M {
  type E
}

def f[T: Type](using QuoteContext) =
  Expr.summon[M] match
    case Some('{ $mm : tt }) =>
      '{
        val m = $mm
        type ME = m.E
        ${ g[ME](using Type[ME]) }
        ${ g[m.E](using Type[ME]) }
        ${ g[ME](using Type[m.E]) }
        ${ g[m.E](using Type[m.E]) }
        // ${ g[ME] } // FIXME: issue seems to be in ReifyQuotes
        // ${ g[m.E] } // FIXME: issue seems to be in ReifyQuotes
      }

def g[T](using Type[T]) = ???
