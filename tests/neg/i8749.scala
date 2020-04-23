import scala.quoted._

object FunObject {
  def fun(t: String => String) = t
}

def test(using QuoteContext)(x: Expr[String => String]) =
  x match
    case '{ FunObject.fun(($arg: String) => $out) } => // error
