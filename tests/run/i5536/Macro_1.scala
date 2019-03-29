import scala.quoted._
import scala.tasty._

object scalatest {
  inline def assert(condition: => Boolean): Unit = ${assertImpl('condition)}

  def assertImpl(condition: Expr[Boolean])(implicit refl: Reflection): Expr[Unit] = {
    import refl._
    import quoted.Toolbox.Default._

    val tree = condition.unseal
    def exprStr: String = condition.show

    tree.underlyingArgument match {
      case Apply(Select(lhs, op), rhs :: Nil) =>
        val left = lhs.seal[Any]
        val right = rhs.seal[Any]
        op match {
          case "===" =>
            '{
              val _left   = $left
              val _right  = $right
              val _result = _left == _right
              scala.Predef.assert(_result)
            }
        }
    }
  }
}
