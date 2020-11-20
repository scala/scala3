import scala.quoted._

object scalatest {

  inline def assert(condition: => Boolean): Unit = ${ assertImpl('condition) }

  def assertImpl(cond: Expr[Boolean])(using qctx: QuoteContext) : Expr[Unit] = {
    import qctx.reflect._
    import util._
    import ValDef.let

    Term.of(cond).underlyingArgument match {
      case t @ Apply(Select(lhs, op), rhs :: Nil) =>
        let(Symbol.currentOwner, lhs) { left =>
          let(Symbol.currentOwner, rhs) { right =>
            val app = Select.overloaded(left, op, Nil, right :: Nil)
            let(Symbol.currentOwner, app) { result =>
              val l = left.asExpr
              val r = right.asExpr
              val b = result.asExprOf[Boolean]
              val code = '{ scala.Predef.assert($b) }
              Term.of(code)
            }
          }
        }.asExprOf[Unit]
    }
  }

}
