
import scala.quoted._

def f(using s: Scope) = {
  val x: s.Expr[Double] = '{5: Double} // FIXME remove ascription
}
