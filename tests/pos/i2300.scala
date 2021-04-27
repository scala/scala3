object hlists {
  sealed abstract class HList {
    type Merge[L <: HList] <: HList

    def merge[L <: HList](l: L): Merge[L]
  }
  final case class HCons[H, T <: HList](head : H, tail : T) extends HList {
    type Merge[L <: HList] = HCons[H, tail.Merge[L]]

    def merge[L <: HList](l: L): Merge[L] = HCons(head, tail.merge(l))
  }
  sealed trait HNil extends HList {
    type Merge[L <: HList] = L

    def merge[L <: HList](l: L): Merge[L] = l
  }
  final val HNil: HNil = { case object HNil extends HNil; HNil }
}

object Test {
  import hlists.*

  val merged: HCons[Int,HCons[String,HNil]] = {
    HCons(42, HNil) merge HCons("foo", HNil)
  }
}
