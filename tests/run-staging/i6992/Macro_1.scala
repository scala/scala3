
import scala.quoted.*
import scala.quoted.staging.*

package macros:

  object macros {
    inline def mcr(x: => Any): Any = ${mcrImpl('x)}

    class Foo { val x = 10 }

    def mcrImpl(body: Expr[Any])(using ctx: Quotes): Expr[Any] =
      MyTest.mcrImpl(body)
  }

package scala {
  object MyTest {
    import macros.macros.*

   given Compiler = Compiler.make(getClass.getClassLoader)

    def mcrImpl(body: Expr[Any])(using ctx: Quotes): Expr[Any] = {
      import ctx.reflect.*
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
