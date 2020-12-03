import scala.quoted._

object Macros {

  inline def fun(x: Any): Unit = ${ impl('x) }

  def impl(x: Expr[Any])(using Quotes) : Expr[Unit] = {
    import quotes.reflect._
    val pos = Term.of(x).underlyingArgument.pos
    report.error("here is the the argument is " + Term.of(x).underlyingArgument.show, pos)
    report.error("here (+5) is the the argument is " + Term.of(x).underlyingArgument.show, Position(pos.sourceFile, pos.start + 5, pos.end + 5))
    '{}
  }

}
