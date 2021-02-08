import scala.quoted.*

object Macros {

  implicit inline def withSource(arg: Any): (String, Any) = ${ impl('arg) }

  private def impl(arg: Expr[Any])(using Quotes) : Expr[(String, Any)] = {
    import quotes.reflect.*
    val source = Expr(arg.asTerm.underlyingArgument.pos.sourceCode.get.toString)
    '{Tuple2($source, $arg)}
  }

}
