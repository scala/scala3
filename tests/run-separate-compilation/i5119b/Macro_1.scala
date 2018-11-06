import scala.quoted._
import scala.tasty.Tasty

object Macro {

  inline def ff(arg1: Any,  arg2: Any): String = ~Macro.impl('(arg1), '(arg2))

  def impl(arg1: Expr[Any], arg2: Expr[Any])(implicit tasty: Tasty): Expr[String] = {
    import tasty._
    (arg1.reflect.underlyingArgument.show + "\n" + arg2.reflect.underlyingArgument.show).toExpr
  }

}
