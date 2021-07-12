import scala.quoted._

object LibB {

  // Requires explicit given definition
  // Advantage: No duplication of code
  // Drawback: Need to explicitly add the Quotes, Type and ToExpr parameters (not too bad)

  sealed trait Opt[+T]
  object Opt:
    given [T: Type: ToExpr](using Quotes): ToExpr[Opt[T]] = ToExprMaker.derived

  case class Sm[T](t: T) extends Opt[T]
  object Sm:
    given [T: Type: ToExpr](using Quotes): ToExpr[Sm[T]] = ToExprMaker.derived

  case object Nn extends Opt[Nothing]:
    given (using Quotes): ToExpr[Nn.type] = ToExprMaker.derived

  import Opt.*

  inline def optTwo = ${optTwoExpr}
  inline def smTwo = ${smTwoExpr}
  inline def none = ${noneExpr}

  private def optTwoExpr(using Quotes): Expr[Opt[Int]] =
    summon[ToExpr[Opt[Int]]].apply(Sm(2))

  private def smTwoExpr(using Quotes): Expr[Sm[Int]] =
    summon[ToExpr[Sm[Int]]].apply(Sm(2))

  private def noneExpr(using Quotes): Expr[Opt[Int]] =
    summon[ToExpr[Nn.type]].apply(Nn)
}

