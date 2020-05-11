import scala.quoted._

object Macros {

  inline def fun(x: Any): Unit = ${ impl('x) }

  def impl(using s: Scope)(x: s.Expr[Any]) : s.Expr[Unit] = {
    import s.tasty._
    val pos = x.underlyingArgument.pos
    error("here is the the argument is " + x.underlyingArgument.show, pos)
    error("here (+5) is the the argument is " + x.underlyingArgument.show, pos.sourceFile, pos.start + 5, pos.end + 5)
    '{}
  }

}
