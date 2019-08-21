import scala.quoted._
import Lifters._

object UnrolledExpr {

  implicit class Unrolled[T: Liftable, It <: Iterable[T]](xs: It) {
    def unrolled: UnrolledExpr[T, It] = new UnrolledExpr(xs)
  }

  // TODO support blocks in the compiler to avoid creating trees of blocks?
  def block[T: Type](stats: Iterable[Expr[_]], expr: Expr[T]) given QuoteContext: Expr[T] = {
    def rec(stats: List[Expr[_]]): Expr[T] = stats match {
      case x :: xs => '{ $x; ${rec(xs)} }
      case Nil => expr
    }
    rec(stats.toList)
  }

}

class UnrolledExpr[T: Liftable, It <: Iterable[T]](xs: It) {
  import UnrolledExpr._

  def foreach[U](f: T => Expr[U]) given QuoteContext: Expr[Unit] = block(xs.map(f), '{})

  def withFilter(f: T => Boolean) given QuoteContext: UnrolledExpr[T, Iterable[T]] = new UnrolledExpr(xs.filter(f))

  def foldLeft[U](acc: Expr[U])(f: (Expr[U], T) => Expr[U]) given QuoteContext: Expr[U] =
    xs.foldLeft(acc)((acc, x) => f(acc, x))
}
