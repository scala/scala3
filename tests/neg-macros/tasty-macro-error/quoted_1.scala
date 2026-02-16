import scala.quoted.*

object Macros {

  inline def fun(x: Any): Unit = ${ impl('x) }

  def impl(x: Expr[Any])(using Quotes) : Expr[Unit] = {
    import quotes.reflect.*
    report.error("here is the argument is " + x.asTerm.underlyingArgument.show, x.asTerm.underlyingArgument.pos)
    '{}
  }

}
