
import scala.quoted._
import scala.quoted.staging._

given Toolbox = Toolbox.make(getClass.getClassLoader)

object macros {
  inline def mcr(x: => Any): Any = ${mcrImpl('x)}

  class Foo { val x = 10 }

  def mcrImpl(using s: Scope)(body: s.Expr[Any]): s.Expr[Any] = {
    import s.tasty._
    try {
      body match {
        case '{$x: Foo} => Expr(run(x).x) // error // error
      }
    } catch {
      case ex: scala.quoted.staging.Toolbox.ToolboxInUse =>
        '{"OK"}
    }
  }
}
