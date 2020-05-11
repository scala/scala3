import scala.quoted._

trait Show[T] {
  def show(x: T): String
}

def showInterpolatorImpl(using s: Scope)(sc: s.Expr[StringContext], argsExpr: s.Expr[Seq[Any]]): s.Expr[String] =
  argsExpr match
    case '{ $arg: $t } =>
    case '[ Int ] => // error
  ???

extension (inline sc: StringContext) inline def show (args: Any*): String = ${ showInterpolatorImpl('sc, 'args) }
