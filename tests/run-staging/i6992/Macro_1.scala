
import scala.quoted._, scala.quoted.matching._
import scala.quoted.staging._
import scala.quoted.{given _}

given Toolbox = Toolbox.make(getClass.getClassLoader)

object macros {
  inline def mcr(x: => Any): Any = ${mcrImpl('x)}

  class Foo { val x = 10 }

  def mcrImpl(body: Expr[Any]) with (ctx: QuoteContext) : Expr[Any] = {
    import ctx.tasty._
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
