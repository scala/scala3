import scala.quoted._

object Macros {

  inline def assert(condition: => Boolean): Unit = ${ assertImpl('{condition}, '{""}) }

  def assertImpl(using s: Scope)(cond: s.Expr[Boolean], clue: s.Expr[Any]): s.Expr[Unit] = {
    import s.tasty._
    val b = cond.underlyingArgument.seal.cast[Boolean]
    '{ scala.Predef.assert($b) }
  }

  inline def thisLineNumber = ${ thisLineNumberImpl }

  def thisLineNumberImpl(using s: Scope): s.Expr[Int] = {
    import s.tasty._
    Expr(rootPosition.startLine)
  }
}
