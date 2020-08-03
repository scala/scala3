import scala.quoted._

class M {
  type E
}

def f[T: Staged](using QuoteContext) =
  Expr.summon[M] match
    case Some('{ $mm : $tt }) =>
      '{
        val m = $mm
        type ME = m.E
        ${ g[ME](using '[ME]) }
        ${ g[m.E](using '[ME]) }
        ${ g[ME](using '[m.E]) }
        ${ g[m.E](using '[m.E]) }
        // ${ g[ME] } // FIXME: issue seems to be in ReifyQuotes
        // ${ g[m.E] } // FIXME: issue seems to be in ReifyQuotes
      }

def g[T](using Staged[T]) = ???
