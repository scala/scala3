package macros
import scala.quoted._

def oops(using QuoteContext) = {
  var v = '{0};
  val q = '{ (x: Int) => ${ v = '{x}; v } }
  '{$q($v)}
}
inline def test = ${oops}
