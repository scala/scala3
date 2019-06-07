import scala.quoted._
import scala.quoted.autolift._

import scala.tasty._

object Macros {

  implicit inline def withSource(arg: Any): (String, Any) = ${ impl('arg) }

  private def impl(arg: Expr[Any])(implicit reflect: Reflection): Expr[(String, Any)] = {
    import reflect._
    val source = arg.unseal.underlyingArgument.pos.sourceCode.toString
    '{Tuple2($source, $arg)}
  }

}
