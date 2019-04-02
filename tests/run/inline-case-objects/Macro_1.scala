
import scala.quoted._
import scala.quoted.autolift._

object Macros {
  def impl(foo: Any): Expr[String] = foo.getClass.getCanonicalName
}

case object Bar {
  case object Baz
}

package foo {
  case object Bar {
    case object Baz
  }
}
