
import scala.quoted._
import scala.quoted.staging._

given Toolbox = Toolbox.make(getClass.getClassLoader)

object macros {
  inline def mcr(x: => Any): Any = ${mcrImpl('x)}

  class Foo { val x = 10 }

  def mcrImpl(body: Expr[Any])(using ctx: QuoteContext): Expr[Any] = {
    import ctx.reflect._
    try {
      body match {
        case '{$x: Foo} => Expr(run(x).x)
      }
    } catch {
      case ex: scala.quoted.ScopeException =>
        '{"OK"}
    }
  }
}
