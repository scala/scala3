package macros
import scala.quoted._

def oops(using Scope) = {
  var v = '{0};
  val q = '{ ??? match { case x => ${ v = '{x}; v } } } // error
  v
}
inline def test = ${oops}
