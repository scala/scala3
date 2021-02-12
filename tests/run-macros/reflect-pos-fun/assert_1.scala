import scala.quoted.*

object scalatest {

  inline def assert(condition: => Boolean): Unit = ${ assertImpl('condition) }

  def assertImpl(cond: Expr[Boolean])(using Quotes) : Expr[Unit] = {
    import quotes.reflect.*
    import util.*

    cond.asTerm.underlyingArgument match {
      case t @ Apply(TypeApply(Select(lhs, op), targs), rhs) =>
        ValDef.let(Symbol.spliceOwner, lhs) { left =>
          ValDef.let(Symbol.spliceOwner, rhs) { rs =>
            val app = Select.overloaded(left, op, targs.map(_.tpe), rs)
            val b = app.asExprOf[Boolean]
            '{ scala.Predef.assert($b) }.asTerm
          }
        }.asExprOf[Unit]
    }
  }
}
