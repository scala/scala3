import scala.quoted._, scala.quoted.matching._
import scala.quoted.given
import scala.tasty._

case class Box[T](v: T)

inline def mcr(expr: => Boolean): Unit = ${mcrProxy('expr)}

def mcrProxy(expr: Expr[Boolean])(given QuoteContext): Expr[Unit] = {
  val res = mcrImpl[Boolean]('{ (esx: Seq[Box[Boolean]]) => () }, expr)
  // println(s"Out: ${res.show}")
  res
}

def mcrImpl[T](func: Expr[Seq[Box[T]] => Unit], expr: Expr[T])(given ctx: QuoteContext, tt: TypeTag[T]): Expr[Unit] = {
  import ctx.tasty._
  val arg = Expr.ofSeq(Seq('{(Box($expr))}))
  Expr.betaReduce(func)(arg)
}