import scala.quoted._
import scala.tasty._

object lib {

  inline def assert(condition: => Boolean): Unit = ${ assertImpl('condition, '{""}) }

  def assertImpl(cond: Expr[Boolean], clue: Expr[Any])(implicit refl: Reflection): Expr[Unit] = {
    import refl._
    import util._

    cond.unseal.underlyingArgument match {
      case t @ Apply(Select(lhs, op), Lambda(param :: Nil, Apply(Select(a, "=="), b :: Nil)) :: Nil)
      if a.symbol == param.symbol || b.symbol == param.symbol =>
        '{ scala.Predef.assert($cond) }
    }
  }
}
