import scala.quoted._

object Macros {

  implicit inline def withSource(arg: Any): (String, Any) = ${ impl('arg) }

  private def impl(using s: Scope)(arg: s.Expr[Any]): s.Expr[(String, Any)] = {
    val source = Expr(arg.underlyingArgument.pos.sourceCode.toString)
    '{Tuple2($source, $arg)}
  }

}
