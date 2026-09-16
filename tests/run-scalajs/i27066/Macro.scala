import scala.quoted.*

def wrap(left: Any): Unit = ()

inline def assert(inline condition: Boolean): Unit =
  ${ assertImpl('{ condition }) }

def assertImpl(condition: Expr[Boolean])(using Quotes): Expr[Unit] =
  import quotes.reflect.*
  import ValDef.let
  condition.asTerm.underlyingArgument match
    case Select(left, _) =>
      let(Symbol.spliceOwner, left) { l =>
        '{ wrap(${ l.asExpr }) }.asTerm
      }.asExprOf[Unit]
