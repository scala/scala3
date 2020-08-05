import scala.quoted._

object Macros {

  implicit inline def withSource(arg: Any): (String, Any) = ${ impl('arg) }

  private def impl(arg: Expr[Any])(using qctx: QuoteContext) : Expr[(String, Any)] = {
    import qctx.tasty._
    val source = Expr(arg.asTerm.underlyingArgument.pos.sourceCode.toString)
    '{Tuple2($source, $arg)}
  }

}
