// Companion to i15798.scala. The fix for #15798 expands transparent
// inline givens at typer phase even inside inline method bodies; do not
// expand givens whose signature has a using-clause, since their body may
// branch on the path of the using arg (e.g. the perspective community
// build's `HKDGeneric.derived`, which uses `inline m match`). Eager
// expansion would lose path information from the implicit argument and
// produce a result that no longer matches the declared type.

import scala.deriving.*
import scala.compiletime.*

sealed trait HKDGeneric[A]:
  type Gen
  def label: String

object HKDGeneric:
  type Aux[A, G] = HKDGeneric[A] { type Gen = G }

  transparent inline given derived[A](using m: Mirror.Of[A]): HKDGeneric[A] = inline m match
    case m: Mirror.ProductOf[A] { type MirroredElemTypes = m.MirroredElemTypes } =>
      HKDProductGeneric.derived[A](using m)
    case _: Mirror.SumOf[A] =>
      ???

trait HKDProductGeneric[A] extends HKDGeneric[A]
object HKDProductGeneric:
  type Aux[A, G] = HKDProductGeneric[A] { type Gen = G }

  transparent inline given derived[A](using m: Mirror.ProductOf[A]): HKDProductGeneric[A] =
    derivedImpl[A, m.MirroredElemTypes, m.MirroredLabel](constValue[m.MirroredLabel])

  def derivedImpl[A, ElemTypes <: Tuple, Label <: String](label0: Label)(
      using m: Mirror.ProductOf[A] { type MirroredElemTypes = ElemTypes; type MirroredLabel = Label }
  ): HKDProductGeneric[A] { type Gen = ElemTypes } =
    new HKDProductGeneric[A]:
      type Gen = ElemTypes
      def label: String = label0


trait Encoder[A]:
  def name: String
object Encoder:
  inline given derived[A](using gen: HKDGeneric[A]): Encoder[A] = inline gen match
    case gen: HKDProductGeneric.Aux[A, gen.Gen] =>
      derivedProductEncoder[A](using gen)
    case _ => ???

  def derivedProductEncoder[A](using gen: HKDProductGeneric[A]): Encoder[A] = new Encoder[A] {
    def name = gen.label
  }

@main def loopTest =
  case class A(value: String) derives Encoder
  println("compiled")
