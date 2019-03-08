// This file should still compile if Config.LogPrendingSubtypesthreshold is set to 9.
sealed trait HList
trait HNil extends HList
trait HCons[+H, +T] extends HList

trait Concat[L1, L2] { type Out }
object Concat {
  implicit def i0[L]:
    Concat[HNil, L] { type Out = L } = null

  implicit def i1[H, T, L, O]
    (implicit c: Concat[T, L] { type Out = O }):
      Concat[HCons[H, T], L] { type Out = HCons[H, O] } = null
}

object Test {
  type L1 = HCons[Unit, HNil]

  implicitly[Concat[L1, L1]]
}