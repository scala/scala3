import scala.quoted._
import scala.quoted.autolift._

object Macros {

  implicit inline def withSource(arg: Any): (String, Any) = ${ impl('arg) }

  private def impl(arg: Expr[Any]) given (qctx: QuoteContext): Expr[(String, Any)] = {
    import qctx.tasty._
    val source = arg.unseal.underlyingArgument.pos.sourceCode.toString
    '{Tuple2($source, $arg)}
  }

}
