import scala.quoted.*

trait Appendable[T]
trait Format[T] { def empty: T }

object FormatMacros {

  def test[T <: Appendable[T]: Type, F <: Format[T]: Type](f: Expr[F])(using Quotes): Expr[T] =
    '{ $f.empty }

  def test2[T <: Appendable[T2]: Type, T2 <: Appendable[T], F <: Format[T]: Type](f: Expr[F])(using Quotes): Expr[T] =
    '{ $f.empty }

}
