package macros
import scala.quoted._

def oops(using Scope) = {
  var v = '{0};
  val q = '{ def f(x: Int): Int = ${ v = '{x}; v } } // error
  v
}
inline def test = ${oops}
