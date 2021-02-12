import scala.quoted.*

object scalatest {

  inline def assert(condition: => Boolean): Unit = ${ assertImpl('condition, '{""}) }

  def assertImpl(cond: Expr[Boolean], clue: Expr[Any])(using Quotes): Expr[Unit] = {
    import quotes.reflect.*
    import util.*

    def isImplicitMethodType(tp: TypeRepr): Boolean = tp match
      case tp: MethodType => tp.isImplicit
      case _ => false

    cond.asTerm.underlyingArgument match {
      case t @ Apply(sel @ Select(lhs, op), rhs :: Nil) =>
        ValDef.let(Symbol.spliceOwner, lhs) { left =>
          ValDef.let(Symbol.spliceOwner, rhs) { right =>
            val app = left.select(sel.symbol).appliedTo(right)
            ValDef.let(Symbol.spliceOwner, app) { result =>
              val l = left.asExpr
              val r = right.asExpr
              val b = result.asExprOf[Boolean]
              val code = '{ scala.Predef.assert($b) }
              code.asTerm
            }
          }
        }.asExprOf[Unit]
      case Apply(f @ Apply(sel @ Select(Apply(qual, lhs :: Nil), op), rhs :: Nil), implicits)
      if isImplicitMethodType(f.tpe) =>
        ValDef.let(Symbol.spliceOwner, lhs) { left =>
          ValDef.let(Symbol.spliceOwner, rhs) { right =>
            val app = qual.appliedTo(left).select(sel.symbol).appliedTo(right)
            ValDef.let(Symbol.spliceOwner, Apply(app, implicits)) { result =>
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
