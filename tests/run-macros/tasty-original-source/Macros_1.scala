import scala.quoted._

object Macros {

  implicit inline def withSource(arg: Any): (String, Any) = ${ impl('arg) }

  private def impl(arg: Expr[Any])(using Quotes) : Expr[(String, Any)] = {
    import qctx.reflect._
    val source = Expr(Term.of(arg).underlyingArgument.pos.sourceCode.toString)
    '{Tuple2($source, $arg)}
  }

}
