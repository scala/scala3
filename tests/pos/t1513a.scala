import language.experimental.namedTypeArguments
object Test {
  // Heterogeneous lists and natural numbers as defined in shapeless.

  sealed trait HList
  sealed trait ::[H, T <: HList] extends HList
  sealed trait HNil extends HList

  sealed trait Nat
  sealed trait Succ[P <: Nat] extends Nat
  sealed trait Zero extends Nat

  // Accessor type class to compute the N'th element of an HList L.

  trait Accessor[L <: HList, N <: Nat] { type Out }
  object Accessor {
    type Aux[L <: HList, N <: Nat, O] = Accessor[L, N] { type Out = O }

    // (H :: T).At[Zero] = H
    implicit def caseZero[H, T <: HList]: Aux[H :: T, Zero, H] = ???

    // T.At[N] = O => (H :: T).At[Succ[N]] = O
    implicit def caseN[H, T <: HList, N <: Nat, O]
      (implicit a: Aux[T, N, O]): Aux[H :: T, Succ[N], O] = ???
  }

  case class Proxy[T]()

  def at1[NN <: Nat, OO]              (implicit e: Accessor.Aux[String :: HNil, NN, OO]): OO = ???
  def at2[NN <: Nat, OO](p: Proxy[NN])(implicit e: Accessor.Aux[String :: HNil, NN, OO]): OO = ???

  // N is fixed by a value
  at2(Proxy[Zero]()): String

  // N is fixed as a type parameter (by name)
  at1[NN = Zero]: String
}
