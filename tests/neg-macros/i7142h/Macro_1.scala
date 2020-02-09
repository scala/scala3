package macros
import scala.quoted._

var saved = Option.empty[Expr[Int]]

def oops(given QuoteContext) = {
  if (saved.isEmpty) '{ (x: Int) => ${ saved = Some('{x}); 'x } }
  else saved.get
}

inline def test = ${oops}
