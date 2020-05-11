package macros
import scala.quoted._

def oops(using Scope) = {
  var v = '{};
  val q = '{ var x: Int = 8; ${ v = '{x = 9}; v } } // error
  v
}
inline def test = ${oops}
