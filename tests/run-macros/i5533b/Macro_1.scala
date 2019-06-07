import scala.quoted._
import scala.tasty._

object scalatest {
  def f(x: Int): Int = x
  def f(x: String): String = x

  inline def assert(condition: => Boolean): Unit = ${assertImpl('condition)}

  def assertImpl(condition: Expr[Boolean])(implicit refl: Reflection): Expr[Unit] = {
    import refl._
    val tree = condition.unseal
    def exprStr: String = condition.show

    tree.underlyingArgument match {
      case Apply(Select(lhs, op), rhs :: Nil) =>
        val left = lhs.seal
        val right = rhs.seal
        op match {
          case "==" =>
        '{
          val _left   = $left
          val _right  = $right
          val _result = _left == _right
          println(_left)
          println(_right)
          scala.Predef.assert(_result)
        }
      }
    }
  }
}
