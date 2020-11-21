
import scala.quoted._
import scala.quoted.staging._


object macros {
  inline def mcr(x: => Any): Any = ${mcrImpl('x)}

  class Foo { val x = 10 }

  def mcrImpl(body: Expr[Any])(using ctx: QuoteContext): Expr[Any] =
    MyTest.mcrImpl(body)
}

package scala {
  object MyTest {
    import macros._

   given Toolbox = Toolbox.make(getClass.getClassLoader)

    def mcrImpl(body: Expr[Any])(using ctx: QuoteContext): Expr[Any] = {
      import ctx.reflect._
      try {
        body match {
          case '{$x: Foo} => Expr(run(x).x)
        }
      } catch {
        case ex: Exception if ex.getClass.getName == "scala.quoted.runtime.impl.ScopeException" =>
          '{"OK"}
      }
    }
  }
}
