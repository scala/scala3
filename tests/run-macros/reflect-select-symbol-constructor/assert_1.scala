import scala.quoted._

object scalatest {

  inline def assert(condition: => Boolean): Unit = ${ assertImpl('condition, '{""}) }

  def assertImpl(using s: Scope)(cond: s.Expr[Boolean], clue: s.Expr[Any]): s.Expr[Unit] = {
    import s.tasty._
    import util._

    def isImplicitMethodType(tp: Type): Boolean = tp match
      case tp: MethodType => tp.isImplicit
      case _ => false

    cond.underlyingArgument match {
      case t @ Apply(sel @ Select(lhs, op), rhs :: Nil) =>
        let(lhs) { left =>
          let(rhs) { right =>
            val app = Apply(Select(left, sel.symbol), right :: Nil)
            let(app) { result =>
              val l = left.seal
              val r = right.seal
              val b = result.seal.cast[Boolean]
              val code = '{ scala.Predef.assert($b) }
              code
            }
          }
        }.seal.cast[Unit]
      case Apply(f @ Apply(sel @ Select(Apply(qual, lhs :: Nil), op), rhs :: Nil), implicits)
      if isImplicitMethodType(f.tpe) =>
        let(lhs) { left =>
          let(rhs) { right =>
            val app = Apply(Select(Apply(qual, left :: Nil), sel.symbol), right :: Nil)
            let(Apply(app, implicits)) { result =>
              val l = left.seal
              val r = right.seal
              val b = result.seal.cast[Boolean]
              val code = '{ scala.Predef.assert($b) }
              code
            }
          }
        }.seal.cast[Unit]
    }
  }

}
