sealed trait HList
final case class HCons[H, T <: HList](head : H, tail : T) extends HList
case object HNil extends HList

object HList {
  type ::[H, T <: HList] = HCons[H, T]
  type HNil = HNil.type

  implicit def hlistOps[L <: HList](l : L): AnyRef{def ::[H](h: H): HList.::[H,L]; def last(implicit last: HList.Last[L]): Unit} = new {
    def ::[H](h : H) : H :: L = HCons(h, l)
    def last(implicit last : Last[L]): Unit = {}
  }

  class Last[L <: HList]
  implicit def hsingleLast[H]: HList.Last[HList.::[H,HList.HNil]] = new Last[H :: HNil]
  implicit def hlistLast[H, T <: HList](implicit lt : Last[T]): HList.Last[HList.::[H,T]] = new Last[H :: T]

  type III = Int :: Int :: Int :: HNil
  val iii : III = 0 :: 0 :: 0 :: HNil
  val l = iii.last
}
