import scala.quoted._

object scalatest {

  inline def assert(condition: => Boolean): Unit = ${ assertImpl('condition, '{""}) }

  def assertImpl(cond: Expr[Boolean], clue: Expr[Any])(using QuoteContext) : Expr[Unit] = {
    import qctx.reflect._
    import util._

    def isImplicitMethodType(tp: TypeRepr): Boolean = tp match
      case tp: MethodType => tp.isImplicit
      case _ => false

    cond.unseal.underlyingArgument match {
      case t @ Apply(Select(lhs, op), rhs :: Nil) =>
        ValDef.let(lhs) { left =>
          ValDef.let(rhs) { right =>
            val app = Select.overloaded(left, op, Nil, right :: Nil)
            ValDef.let(app) { result =>
              val l = left.seal
              val r = right.seal
              val b = result.seal.cast[Boolean]
              val code = '{ scala.Predef.assert($b) }
              code.unseal
            }
          }
        }.seal.cast[Unit]
      case Apply(f @ Apply(Select(Apply(qual, lhs :: Nil), op), rhs :: Nil), implicits)
      if isImplicitMethodType(f.tpe) =>
        ValDef.let(lhs) { left =>
          ValDef.let(rhs) { right =>
            val app = Select.overloaded(Apply(qual, left :: Nil), op, Nil, right :: Nil)
            ValDef.let(Apply(app, implicits)) { result =>
              val l = left.seal
              val r = right.seal
              val b = result.seal.cast[Boolean]
              val code = '{ scala.Predef.assert($b) }
              code.unseal
            }
          }
        }.seal.cast[Unit]
    }
  }

}
