
sealed trait Opt[+T] derives Lft
case class Sm[T](t: T) extends Opt[T] derives Lft
case object Nn extends Opt[Nothing] derives Lft

object Lib {

  import scala.quoted._
  import Opt.{given _}

  inline def smTwo = ${smTwoExpr}
  inline def none = ${noneExpr}

  private def smTwoExpr(using QuoteContext): Expr[Opt[Int]] =
    summon[Lft[Sm[Int]]].toExpr(Sm(2))

  private def noneExpr(using QuoteContext): Expr[Opt[Int]] =
    summon[Lft[Nn.type]].toExpr(Nn)
}

