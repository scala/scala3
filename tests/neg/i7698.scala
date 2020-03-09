import scala.quoted._
import scala.quoted.matching._

trait Show[T] {
  def show(x: T): String
}

def showInterpolatorImpl(sc: Expr[StringContext], argsExpr: Expr[Seq[Any]])(using qctx: QuoteContext): Expr[String] =
  argsExpr.unseal match
    case '{ $arg: $t } => // error
    case '[ Int ] => // error
  ???

inline def (sc: => StringContext) show (args: Any*): String = ${ showInterpolatorImpl('sc, 'args) }
