package macros
import scala.quoted._

def oops(using Scope) = {
  var v = '{0};
  val q = '{ def x: Int = 8; ${ v = '{x}; v } } // error
  v
}
inline def test = ${oops}
