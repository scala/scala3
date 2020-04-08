import scala.quoted._


object Macros {

  inline def g(inline x: Unit): Unit = ${impl('x)}

  private def impl(x: Expr[Any])(using QuoteContext): Expr[Any] = {
    x match
      case '{ println(f($y)) } => y
  }

}

def f[T](x: T): T = x
