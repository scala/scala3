import scala.quoted._

def f(using QuoteContext)(x: Expr[Any]): Unit = {
  x match
    case '{ val a = 3; $b(a): Int } => // error: Possibly using old-higher order pattern syntax instead application pattern.
}