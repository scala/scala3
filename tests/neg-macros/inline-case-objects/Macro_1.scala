
import scala.quoted._
import scala.quoted.autolift._

object Macros {
  def impl(foo: Any): Expr[String] = foo.getClass.getCanonicalName
}

class Bar {
  case object Baz
}

package foo {
  class Bar {
    case object Baz
  }
}
