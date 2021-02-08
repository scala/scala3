import scala.quoted.*

object FunObject {
  def fun(t: String => String) = t
}

def test(using Quotes)(x: Expr[String => String]) =
  x match
    case '{ FunObject.fun(($arg: String) => $out) } => // error
