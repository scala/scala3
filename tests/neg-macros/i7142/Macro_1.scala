package macros
import scala.quoted._

def oops(using Scope) = {
  var v = '{0};
  val q = '{ (x: Int) => ${ v = '{x}; v } } // error
  '{$q($v)}
}
inline def test = ${oops}
