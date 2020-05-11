import scala.quoted._

object scalatest {

  inline def assert(condition: => Boolean): Unit = ${ assertImpl('condition) }

  def assertImpl(using s: Scope)(cond: s.Expr[Boolean]): s.Expr[Unit] = {
    import s.tasty._
    import util._
    cond.underlyingArgument match {
      case t @ Apply(Select(lhs, op), rhs :: Nil) =>
        let(lhs) { left =>
          let(rhs) { right =>
            val app = Select.overloaded(left, op, Nil, right :: Nil)
            let(app) { result =>
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
