import scala.quoted._

object LibA {

  // Requires explicit inline given definition
  // Advantage: simple definition of the extension (slightly smaller than quoted-ToExpr-derivation-macro-2)
  // Drawback: the derived code will be duplicated at each use site

  sealed trait Opt[+T]
  object Opt:
    transparent inline given [T]: ToExpr[Opt[T]] = ToExprMaker.derived

  case class Sm[T](t: T) extends Opt[T]
  object Sm:
    transparent inline given [T]: ToExpr[Sm[T]] = ToExprMaker.derived

  case object Nn extends Opt[Nothing]:
    transparent inline given ToExpr[Nn.type] = ToExprMaker.derived

  import Opt.*

  transparent inline def optTwo = ${optTwoExpr}
  transparent inline def smTwo = ${smTwoExpr}
  transparent inline def none = ${noneExpr}

  private def optTwoExpr(using Quotes): Expr[Opt[Int]] =
    summon[ToExpr[Opt[Int]]].apply(Sm(2))

  private def smTwoExpr(using Quotes): Expr[Sm[Int]] =
    summon[ToExpr[Sm[Int]]].apply(Sm(2))

  private def noneExpr(using Quotes): Expr[Opt[Int]] =
    summon[ToExpr[Nn.type]].apply(Nn)
}

