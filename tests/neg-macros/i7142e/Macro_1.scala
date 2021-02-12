package macros
import scala.quoted.*

def oops(using Quotes) = {
  var v = '{0};
  val q = '{ def x: Int = 8; ${ v = '{x}; v } }
  v
}
inline def test = ${oops}
