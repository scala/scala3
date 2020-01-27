
import scala.quoted._
import scala.quoted.matching._

class M {
  type E
}

def f(using QuoteContext): Expr[Any] =
  val mm: Expr[M] = ???
  '{
    val m: M = $mm
    type ME = m.E
    ${ g[ME](using '[ME]) }
    ${ g[m.E](using '[ME]) }
    ${ g[ME](using '[m.E]) }
    ${ g[m.E](using '[m.E]) }
    // ${ g[ME] } // FIXME
    // ${ g[m.E] } // FIXME
    ${ g(using '[ME]) }
    ${ g(using '[m.E]) }
  }


def g[T](using Type[T]) = ???
