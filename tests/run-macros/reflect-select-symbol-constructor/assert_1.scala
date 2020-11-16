import scala.quoted._

object scalatest {

  inline def assert(condition: => Boolean): Unit = ${ assertImpl('condition, '{""}) }

  def assertImpl(cond: Expr[Boolean], clue: Expr[Any])(using qctx: QuoteContext) : Expr[Unit] = {
    import qctx.reflect._
    import util._

    def isImplicitMethodType(tp: TypeRepr): Boolean = tp match
      case tp: MethodType => tp.isImplicit
      case _ => false

    Term.of(cond).underlyingArgument match {
      case t @ Apply(sel @ Select(lhs, op), rhs :: Nil) =>
        ValDef.let(lhs) { left =>
          ValDef.let(rhs) { right =>
            val app = Apply(Select(left, sel.symbol), right :: Nil)
            ValDef.let(app) { result =>
              val l = left.asExpr
              val r = right.asExpr
              val b = result.asExprOf[Boolean]
              val code = '{ scala.Predef.assert($b) }
              Term.of(code)
            }
          }
        }.asExprOf[Unit]
      case Apply(f @ Apply(sel @ Select(Apply(qual, lhs :: Nil), op), rhs :: Nil), implicits)
      if isImplicitMethodType(f.tpe) =>
        ValDef.let(lhs) { left =>
          ValDef.let(rhs) { right =>
            val app = Apply(Select(Apply(qual, left :: Nil), sel.symbol), right :: Nil)
            ValDef.let(Apply(app, implicits)) { result =>
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
