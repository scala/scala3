import scala.quoted.*

object Macros {

  inline def fun(x: Any): Unit = ${ impl('x) }

  def impl(x: Expr[Any])(using Quotes) : Expr[Unit] = {
    import quotes.reflect.*
    val pos = x.asTerm.underlyingArgument.pos
    report.error("here is the argument is " + x.asTerm.underlyingArgument.show, pos)
    report.error("here (+5) is the argument is " + x.asTerm.underlyingArgument.show, Position(pos.sourceFile, pos.start + 5, pos.end + 5))
    '{}
  }

}
