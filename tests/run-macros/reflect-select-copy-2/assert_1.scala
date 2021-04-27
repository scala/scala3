import scala.quoted.*

object scalatest {

  inline def assert(condition: => Boolean): Unit = ${ assertImpl('condition, '{""}) }

  def assertImpl(cond: Expr[Boolean], clue: Expr[Any])(using Quotes) : Expr[Unit] = {
    import quotes.reflect.*
    import util.*

    def isImplicitMethodType(tp: TypeRepr): Boolean = tp match
      case tp: MethodType => tp.isImplicit
      case _ => false

    cond.asTerm.underlyingArgument match {
      case Apply(sel @ Select(lhs, op), rhs :: Nil) =>
        ValDef.let(Symbol.spliceOwner, lhs) { left =>
          ValDef.let(Symbol.spliceOwner, rhs) { right =>
            ValDef.let(Symbol.spliceOwner, Apply(Select.copy(sel)(left, op), right :: Nil)) { result =>
              val l = left.asExpr
              val r = right.asExpr
              val b = result.asExprOf[Boolean]
              val code = '{ scala.Predef.assert(${b}) }
              code.asTerm
            }
          }
        }.asExprOf[Unit]
      case Apply(f @ Apply(sel @ Select(Apply(qual, lhs :: Nil), op), rhs :: Nil), implicits)
      if isImplicitMethodType(f.tpe) =>
        ValDef.let(Symbol.spliceOwner, lhs) { left =>
          ValDef.let(Symbol.spliceOwner, rhs) { right =>
            ValDef.let(Symbol.spliceOwner, Apply(Apply(Select.copy(sel)(Apply(qual, left :: Nil), op), right :: Nil), implicits)) { result =>
              val l = left.asExpr
              val r = right.asExpr
              val b = result.asExprOf[Boolean]
              val code = '{ scala.Predef.assert(${b}) }
              code.asTerm
            }
          }
        }.asExprOf[Unit]
    }
  }

}
