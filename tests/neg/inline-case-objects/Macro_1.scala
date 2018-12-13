
import scala.quoted._

object Macros {
  def impl(foo: Any): Expr[String] = foo.getClass.getCanonicalName.nn.toExpr
}

class Bar {
  case object Baz
}

package foo {
  class Bar {
    case object Baz
  }
}
