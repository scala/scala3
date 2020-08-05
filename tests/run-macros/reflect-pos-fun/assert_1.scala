import scala.quoted._

object scalatest {

  inline def assert(condition: => Boolean): Unit = ${ assertImpl('condition) }

  def assertImpl(cond: Expr[Boolean])(using qctx: QuoteContext) : Expr[Unit] = {
    import qctx.tasty._
    import util._

    cond.asTerm.underlyingArgument match {
      case t @ Apply(TypeApply(Select(lhs, op), targs), rhs) =>
        let(lhs) { left =>
          lets(rhs) { rs =>
            val app = Select.overloaded(left, op, targs.map(_.tpe), rs)
            val b = app.asExprOf[Boolean]
            '{ scala.Predef.assert($b) }.asTerm
          }
        }.asExprOf[Unit]
    }
  }
}
