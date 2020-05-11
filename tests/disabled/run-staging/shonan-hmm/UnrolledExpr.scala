import scala.quoted._
import Lifters._

object UnrolledExpr {

  implicit class Unrolled[T: Liftable, It <: Iterable[T]](xs: It) {
    def unrolled: UnrolledExpr[T, It] = new UnrolledExpr(xs)
  }

  // TODO support blocks in the compiler to avoid creating trees of blocks?
  def block[T](using s: Scope)(stats: Iterable[s.Expr[_]], expr: Expr[T])(using s.Type[T]): s.Expr[T] = {
    def rec(stats: List[s.Expr[Any]]): s.Expr[T] = stats match {
      case x :: xs => '{ $x; ${rec(xs)} }
      case Nil => expr
    }
    rec(stats.toList)
  }

}

class UnrolledExpr[T: Liftable, It <: Iterable[T]](xs: It) {
  import UnrolledExpr._

  def foreach[U](using s: Scope)(f: T => Expr[U]): s.Expr[Unit] = block(xs.map(f), '{})

  def withFilter(using s: Scope)(f: T => Boolean): UnrolledExpr[T, Iterable[T]] = new UnrolledExpr(xs.filter(f))

  def foldLeft[U](using s: Scope)(acc: s.Expr[U])(f: (s.Expr[U], T) => s.Expr[U]): s.Expr[U] =
    xs.foldLeft(acc)((acc, x) => f(acc, x))
}
