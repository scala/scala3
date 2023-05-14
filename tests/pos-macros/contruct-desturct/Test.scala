import scala.quoted.*
import scala.quoted.util.*

def test(using Quotes) =
  '{ Tuple2(1, 2) } match
    case DestructExpr((a, b)) =>
      ConstructExpr[(Int, Int)].from((a, b))
