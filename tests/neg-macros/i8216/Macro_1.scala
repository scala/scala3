package macros
import scala.quoted._

var saved = Option.empty[Expr[Any]]

def oops(c: Expr[Any])(using Quotes) = {
  if saved.isEmpty then
    saved = Some(c)
    c
  else saved.get
}
inline def test(c: Any) = ${oops('c)}
