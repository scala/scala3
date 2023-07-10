import scala.quoted.*

object Macro:
  transparent inline def foo: Any = ${ fooImpl }

  def fooImpl(using Quotes): Expr[Any] =
    import quotes.reflect.*
    '{
        val xxx = ${
          Type.of[Int] match
            case '[tpe] =>
              Typed(Expr(1).asTerm, TypeTree.of[tpe]).asExpr
        }
        xxx
    }
