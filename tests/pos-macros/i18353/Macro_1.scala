import scala.compiletime.*
import scala.deriving.*
import scala.quoted.*

trait Getter[S, A]:
  def view: S => A

trait Lens[S, A] extends Getter[S, A]:
  def set: S => A => S

object Lens {
  inline def apply[S, A](_view: S => A)(_set: S => A => S): Lens[S, A] =
    new Lens[S, A]:
      def view: S => A = _view
      def set: S => A => S = _set

  inline given derived[T <: Product, A]: Lens[T, A] = ${
    ProductMacros.genLens[T, A]
  }
}

object ProductMacros {
  private def indexOf[T: Type, A: Type](using Quotes): Int =
    indexOf0[T, A](0)

  private def indexOf0[T: Type, A: Type](acc: Int)(using Quotes): Int =
    Type.of[T] match
      case '[EmptyTuple]  => -1
      case '[A *: tpes]   => acc
      case '[tpe *: tpes] => indexOf0[tpes, A](acc + 1)

  def genLens[T <: Product: Type, A: Type](using
      q: Quotes
  ): Expr[Lens[T, A]] = {
    import quotes.reflect.*

    Expr
      .summon[Mirror.ProductOf[T]]
      .map {
        case '{
              $m: Mirror.ProductOf[T] { type MirroredElemTypes = elementTypes }
            } =>
          val i = indexOf[elementTypes, A]
          if i < 0 then
            report.errorAndAbort(s"has no the field of ${Type.show[A]}")
          else
            val ii: Expr[Int] = Expr(i)
            val view: Expr[T => A] = '{ t =>
              t.productElement($ii).asInstanceOf[A]
            }
            val set: Expr[T => A => T] = '{ t => a =>
              val arr = Tuple.fromProduct(t).toArray
              arr($ii) = a.asInstanceOf[Object]
              // Check-macros fails here probably
              $m.fromTuple(Tuple.fromArray(arr).asInstanceOf[elementTypes])
            }
            '{ Lens[T, A]($view)($set) }
      }
      .getOrElse(
        report.errorAndAbort(s"${Type.show[T]} is not a product type")
      )

  }
}
