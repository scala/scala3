import scala.quoted.*

object scalatest {

  inline def assert(condition: => Boolean): Unit = ${ assertImpl('condition) }

  def assertImpl(cond: Expr[Boolean])(using Quotes) : Expr[Unit] = {
    import quotes.reflect.*
    import util.*
    import ValDef.let

    cond.asTerm.underlyingArgument match {
      case t @ Apply(Select(lhs, op), rhs :: Nil) =>
        let(Symbol.spliceOwner, lhs) { left =>
          let(Symbol.spliceOwner, rhs) { right =>
            val app = Select.overloaded(left, op, Nil, right :: Nil)
            let(Symbol.spliceOwner, app) { result =>
              val l = left.asExpr
              val r = right.asExpr
              val b = result.asExprOf[Boolean]
              val code = '{ scala.Predef.assert($b) }
              code.asTerm
            }
          }
        }.asExprOf[Unit]
    }
  }

}
