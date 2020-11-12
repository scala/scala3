import scala.quoted._

object scalatest {

  inline def assert(condition: => Boolean): Unit = ${ assertImpl('condition) }

  def assertImpl(cond: Expr[Boolean])(using qctx: QuoteContext) : Expr[Unit] = {
    import qctx.reflect._
    import util._
    import ValDef.let

    cond.unseal.underlyingArgument match {
      case t @ Apply(Select(lhs, op), rhs :: Nil) =>
        let(lhs) { left =>
          let(rhs) { right =>
            val app = Select.overloaded(left, op, Nil, right :: Nil)
            let(app) { result =>
              val l = left.asExpr
              val r = right.asExpr
              val b = result.asExprOf[Boolean]
              val code = '{ scala.Predef.assert($b) }
              code.unseal
            }
          }
        }.asExprOf[Unit]
    }
  }

}
