
import scala.quoted.{_, given}

object Macros {
  def impl(foo: Any)(given QuoteContext): Expr[String] = foo.getClass.getCanonicalName.toExpr
}

class Bar {
  case object Baz
}

package foo {
  class Bar {
    case object Baz
  }
}
