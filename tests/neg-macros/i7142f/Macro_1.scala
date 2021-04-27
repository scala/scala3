package macros
import scala.quoted.*

def oops(using Quotes) = {
  var v = '{0};
  val q = '{ def f(x: Int): Int = ${ v = '{x}; v } }
  v
}
inline def test = ${oops}
