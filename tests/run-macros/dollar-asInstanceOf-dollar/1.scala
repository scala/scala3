import scala.quoted._
import scala.deriving._

case class Box[A](x: A)

object Macro {
  inline def foo[X[_]](implicit inline m: Mirror { type MirroredType = X }): Int =
    ${ fooImpl }

  def fooImpl[X[_]](implicit m: Mirror { type MirroredType = X }, qc: QuoteContext): Expr[Int] =
    '{ 1 }
}
