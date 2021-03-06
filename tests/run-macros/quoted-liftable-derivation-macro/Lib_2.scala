
sealed trait Opt[+T] derives Lft
case class Sm[T](t: T) extends Opt[T] derives Lft
case object Nn extends Opt[Nothing] derives Lft

object Lib {

  import scala.quoted._
  import Opt.*

  inline def optTwo = ${optTwoExpr}
  inline def smTwo = ${smTwoExpr}
  inline def none = ${noneExpr}

  private def optTwoExpr(using Quotes): Expr[Opt[Int]] =
    summon[Lft[Opt[Int]]].toExpr(Sm(2))

  private def smTwoExpr(using Quotes): Expr[Sm[Int]] =
    summon[Lft[Sm[Int]]].toExpr(Sm(2))

  private def noneExpr(using Quotes): Expr[Opt[Int]] =
    summon[Lft[Nn.type]].toExpr(Nn)
}

