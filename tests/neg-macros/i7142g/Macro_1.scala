package macros
import scala.quoted._

def oops(using QuoteContext) = {
  var v = '{};
  val q = '{ var x: Int = 8; ${ v = '{x = 9}; v } }
  v
}
inline def test = ${oops}
