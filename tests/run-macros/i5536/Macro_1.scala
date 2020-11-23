import scala.quoted._

object scalatest {
  inline def assert(condition: => Boolean): Unit = ${assertImpl('condition)}

  def assertImpl(condition: Expr[Boolean])(using Quotes) : Expr[Unit] = {
    import qctx.reflect._
    val tree = Term.of(condition)
    def exprStr: String = condition.show

    tree.underlyingArgument match {
      case Apply(Select(lhs, op), rhs :: Nil) =>
        val left = lhs.asExpr
        val right = rhs.asExpr
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
