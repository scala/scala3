
import scala.quoted._
import scala.quoted.matching._

class M {
  type E
}

def f[T: Type](using QuoteContext) =
  summonExpr[M] match
    case Some('{ $mm : $tt }) =>
      '{
        val m = $mm
        ${ val b: m.type =
          m // error
          ???
        }
      }


def g[T](using Type[T]) = ???
