
import scala.quoted.*

object Macros {
  def impl(expr: Expr[Any]) (using Quotes): Expr[String] =
    val obj = expr match {
      case '{ None } => None
      case '{ scala.collection.immutable.Nil } => Nil
      case '{ Bar } => Bar
      case '{ Bar.Baz } => Bar.Baz
      case '{ foo.Bar } => foo.Bar
      case '{ foo.Bar.Baz } => foo.Bar.Baz
    }
    Expr(obj.getClass.getCanonicalName)
}

case object Bar {
  case object Baz
}

package foo {
  case object Bar {
    case object Baz
  }
}
