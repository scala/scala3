package macros
import scala.quoted._

var saved = Option.empty[Expr[Int]]

def oops(using Quotes) = {
  if (saved.isEmpty) '{ (x: Int) => ${ saved = Some('{x}); 'x } }
  else saved.get
}

inline def test = ${oops}
