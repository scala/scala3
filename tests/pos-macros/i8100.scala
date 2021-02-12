import scala.quoted.*

class M {
  type E
}

def f[T: Type](using Quotes) =
  Expr.summon[M] match
    case Some('{ $mm : tt }) =>
      '{
        val m = $mm
        type ME = m.E
        ${ g[ME](using Type.of[ME]) }
        ${ g[m.E](using Type.of[ME]) }
        ${ g[ME](using Type.of[m.E]) }
        ${ g[m.E](using Type.of[m.E]) }
        // ${ g[ME] } // FIXME: issue seems to be in PickleQuotes
        // ${ g[m.E] } // FIXME: issue seems to be in PickleQuotes
      }

def g[T](using Type[T]) = ???
