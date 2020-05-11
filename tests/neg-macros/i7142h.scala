package macros
import scala.quoted._

var saved = Option.empty[Scope#Expr[Int]]

def oops(using s: Scope): s.Expr[Int] = {
  if (saved.isEmpty) '{ (x: Int) => ${ saved = Some('{x}); 'x } } // error
  else saved.get // error
}

inline def test = ${oops}
