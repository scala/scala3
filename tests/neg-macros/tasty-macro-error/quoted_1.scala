import scala.quoted._

object Macros {

  inline def fun(x: Any): Unit = ${ impl('x) }

  def impl(using s: Scope)(x: s.Expr[Any]): s.Expr[Unit] = {
    import s.tasty._
    error("here is the the argument is " + x.underlyingArgument.show, x.underlyingArgument.pos)
    '{}
  }

}
