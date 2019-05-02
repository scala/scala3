
import scala.quoted._
import scala.quoted.autolift._

object Macros {
  inline def fooString(inline x: Any): String = ${Macros.impl(x)}
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
