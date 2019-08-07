
import scala.quoted._, scala.quoted.matching._
import delegate scala.quoted._

delegate for Toolbox = Toolbox.make(getClass.getClassLoader)

object macros {
  inline def mcr(x: => Any): Any = ${mcrImpl('x)}

  class Foo { val x = 10 }

  def mcrImpl(body: Expr[Any]) given (ctx: QuoteContext): Expr[Any] = {
    import ctx.tasty._
    try {
      body match {
        case '{$x: Foo} => run(x).x.toExpr
      }
    } catch {
      case _: scala.quoted.Toolbox.RunScopeException =>
        '{"OK"}
    }
  }
}
