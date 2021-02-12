package macros
import scala.quoted.*

def oops(using Quotes) = {
  var v = '{0};
  val q = '{ (x: Int) => ${ v = '{x}; v } }
  '{$q($v)}
}
inline def test = ${oops}
