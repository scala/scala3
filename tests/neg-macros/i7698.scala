import scala.quoted.*

trait Show[T] {
  def show(x: T): String
}

def showInterpolatorImpl(sc: Expr[StringContext], argsExpr: Expr[Seq[Any]])(using Quotes): Expr[String] =
  import quotes.reflect.*
  argsExpr.asTerm match
    case '{ $arg: $t } => // error
    case '[ Int ] => // error
  ???

extension (inline sc: StringContext) inline def show (args: Any*): String = ${ showInterpolatorImpl('sc, 'args) }
