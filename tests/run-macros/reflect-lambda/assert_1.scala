import scala.quoted._

object lib {

  inline def assert(condition: => Boolean): Unit = ${ assertImpl('condition, '{""}) }

  def assertImpl(using s: Scope)(cond: s.Expr[Boolean], clue: s.Expr[Any]): s.Expr[Unit] = {
    import s.tasty._
    import util._

    cond.underlyingArgument match {
      case t @ Apply(Select(lhs, op), Lambda(param :: Nil, Apply(Select(a, "=="), b :: Nil)) :: Nil)
      if a.symbol == param.symbol || b.symbol == param.symbol =>
        '{ scala.Predef.assert($cond) }
    }
  }
}
