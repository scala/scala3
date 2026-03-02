// Compiled with ./build/pack/bin/scalac -J-Xss4M -J-Xmx2G test/induction/inductive-implicits-bench.scala
//
// 1: baseline - scalac 2.12.5
// 2:            scalac 2.12.5 with matchesPtInst
//
//              (1)   (2)
// HList Size
//  50           4     3
// 100           7     4
// 150          14     4
// 200          25     5
// 250          46     5
// 300          74     6
// 350         118     8
// 400         183    10
// 450         298    12
// 500         421    15
//
//            Compile time in seconds

package shapeless {
  sealed trait HList extends Product with Serializable

  final case class ::[+H, +T <: HList](head : H, tail : T) extends HList {
    def ::[HH](h : HH) : HH :: H :: T = shapeless.::(h, this)

    override def toString = head match {
      case _: ::[_, _] => "("+head.toString+") :: "+tail.toString
      case _ => head.toString+" :: "+tail.toString
    }
  }

  sealed trait HNil extends HList {
    def ::[H](h : H) = shapeless.::(h, this)
    override def toString = "HNil"
  }

  case object HNil extends HNil

  //@annotation.inductive
  trait Selector[L <: HList, U] {
    def apply(l: L): U
  }

  object Selector {
    def apply[L <: HList, U](implicit selector: Selector[L, U]): Selector[L, U] = selector

    implicit def inHead[H, T <: HList]: Selector[H :: T, H] =
      new Selector[H :: T, H] {
        def apply(l : H :: T) = l.head
      }

    implicit def inTail[H, T <: HList, U]
      (implicit st : Selector[T, U]): Selector[H :: T, U] =
        new Selector[H :: T, U] {
          def apply(l : H :: T) = st(l.tail)
        }
  }
}

import shapeless.*

object Test extends App {
  val sel = Selector[L, Boolean]

  type L =
    Int ::
    Int ::
    Int ::
    Int ::
    Int ::
    Int ::
    Int ::
    Int ::
    Int ::
    Int ::
//
    Int ::
    Int ::
    Int ::
    Int ::
    Int ::
    Int ::
    Int ::
    Int ::
    Int ::
    Int ::
//
    Int ::
    Int ::
    Int ::
    Int ::
    Int ::
    Int ::
    Int ::
    Int ::
    Int ::
    Int ::
//
    Int ::
    Int ::
    Int ::
    Int ::
    Int ::
    Int ::
    Int ::
    Int ::
    Int ::
    Int ::
//
    Int ::
    Int ::
    Int ::
    Int ::
    Int ::
    Int ::
    Int ::
    Int ::
    Int ::
    Int ::
//
    Int ::
    Int ::
    Int ::
    Int ::
    Int ::
    Int ::
    Int ::
    Int ::
    Int ::
    Int ::
//
    Int ::
    Int ::
    Int ::
    Int ::
    Int ::
    Int ::
    Int ::
    Int ::
    Int ::
    Int ::
//
    Int ::
    Int ::
    Int ::
    Int ::
    Int ::
    Int ::
    Int ::
    Int ::
    Int ::
    Int ::
//
    Int ::
    Int ::
    Int ::
    Int ::
    Int ::
    Int ::
    Int ::
    Int ::
    Int ::
    Int ::
//
    Int ::
    Int ::
    Int ::
    Int ::
    Int ::
    Int ::
    Int ::
    Int ::
    Int ::
    Int ::
//
//    Int ::
//    Int ::
//    Int ::
//    Int ::
//    Int ::
//    Int ::
//    Int ::
//    Int ::
//    Int ::
//    Int ::
// //
//     Int ::
//     Int ::
//     Int ::
//     Int ::
//     Int ::
//     Int ::
//     Int ::
//     Int ::
//     Int ::
//     Int ::
// //
//     Int ::
//     Int ::
//     Int ::
//     Int ::
//     Int ::
//     Int ::
//     Int ::
//     Int ::
//     Int ::
//     Int ::
// //
//     Int ::
//     Int ::
//     Int ::
//     Int ::
//     Int ::
//     Int ::
//     Int ::
//     Int ::
//     Int ::
//     Int ::
// //
//     Int ::
//     Int ::
//     Int ::
//     Int ::
//     Int ::
//     Int ::
//     Int ::
//     Int ::
//     Int ::
//     Int ::
// //
//     Int ::
//     Int ::
//     Int ::
//     Int ::
//     Int ::
//     Int ::
//     Int ::
//     Int ::
//     Int ::
//     Int ::
// //
//     Int ::
//     Int ::
//     Int ::
//     Int ::
//     Int ::
//     Int ::
//     Int ::
//     Int ::
//     Int ::
//     Int ::
// //
//     Int ::
//     Int ::
//     Int ::
//     Int ::
//     Int ::
//     Int ::
//     Int ::
//     Int ::
//     Int ::
//     Int ::
// //
//     Int ::
//     Int ::
//     Int ::
//     Int ::
//     Int ::
//     Int ::
//     Int ::
//     Int ::
//     Int ::
//     Int ::
// //
//     Int ::
//     Int ::
//     Int ::
//     Int ::
//     Int ::
//     Int ::
//     Int ::
//     Int ::
//     Int ::
//     Int ::
//
//     Int ::
//     Int ::
//     Int ::
//     Int ::
//     Int ::
//     Int ::
//     Int ::
//     Int ::
//     Int ::
//     Int ::
// //
//     Int ::
//     Int ::
//     Int ::
//     Int ::
//     Int ::
//     Int ::
//     Int ::
//     Int ::
//     Int ::
//     Int ::
// //
//     Int ::
//     Int ::
//     Int ::
//     Int ::
//     Int ::
//     Int ::
//     Int ::
//     Int ::
//     Int ::
//     Int ::
// //
//     Int ::
//     Int ::
//     Int ::
//     Int ::
//     Int ::
//     Int ::
//     Int ::
//     Int ::
//     Int ::
//     Int ::
// //
//     Int ::
//     Int ::
//     Int ::
//     Int ::
//     Int ::
//     Int ::
//     Int ::
//     Int ::
//     Int ::
//     Int ::
// //
//     Int ::
//     Int ::
//     Int ::
//     Int ::
//     Int ::
//     Int ::
//     Int ::
//     Int ::
//     Int ::
//     Int ::
// //
//     Int ::
//     Int ::
//     Int ::
//     Int ::
//     Int ::
//     Int ::
//     Int ::
//     Int ::
//     Int ::
//     Int ::
// //
//     Int ::
//     Int ::
//     Int ::
//     Int ::
//     Int ::
//     Int ::
//     Int ::
//     Int ::
//     Int ::
//     Int ::
// //
//     Int ::
//     Int ::
//     Int ::
//     Int ::
//     Int ::
//     Int ::
//     Int ::
//     Int ::
//     Int ::
//     Int ::
// //
//     Int ::
//     Int ::
//     Int ::
//     Int ::
//     Int ::
//     Int ::
//     Int ::
//     Int ::
//     Int ::
//     Int ::
// //
//     Int ::
//     Int ::
//     Int ::
//     Int ::
//     Int ::
//     Int ::
//     Int ::
//     Int ::
//     Int ::
//     Int ::
// //
//     Int ::
//     Int ::
//     Int ::
//     Int ::
//     Int ::
//     Int ::
//     Int ::
//     Int ::
//     Int ::
//     Int ::
// //
//     Int ::
//     Int ::
//     Int ::
//     Int ::
//     Int ::
//     Int ::
//     Int ::
//     Int ::
//     Int ::
//     Int ::
// //
//     Int ::
//     Int ::
//     Int ::
//     Int ::
//     Int ::
//     Int ::
//     Int ::
//     Int ::
//     Int ::
//     Int ::
// //
//     Int ::
//     Int ::
//     Int ::
//     Int ::
//     Int ::
//     Int ::
//     Int ::
//     Int ::
//     Int ::
//     Int ::
// //
//     Int ::
//     Int ::
//     Int ::
//     Int ::
//     Int ::
//     Int ::
//     Int ::
//     Int ::
//     Int ::
//     Int ::
// //
//     Int ::
//     Int ::
//     Int ::
//     Int ::
//     Int ::
//     Int ::
//     Int ::
//     Int ::
//     Int ::
//     Int ::
// //
//     Int ::
//     Int ::
//     Int ::
//     Int ::
//     Int ::
//     Int ::
//     Int ::
//     Int ::
//     Int ::
//     Int ::
// //
//     Int ::
//     Int ::
//     Int ::
//     Int ::
//     Int ::
//     Int ::
//     Int ::
//     Int ::
//     Int ::
//     Int ::
// //
//     Int ::
//     Int ::
//     Int ::
//     Int ::
//     Int ::
//     Int ::
//     Int ::
//     Int ::
//     Int ::
//     Int ::
// //
//     Int ::
//     Int ::
//     Int ::
//     Int ::
//     Int ::
//     Int ::
//     Int ::
//     Int ::
//     Int ::
//     Int ::
// //
//     Int ::
//     Int ::
//     Int ::
//     Int ::
//     Int ::
//     Int ::
//     Int ::
//     Int ::
//     Int ::
//     Int ::
// //
//     Int ::
//     Int ::
//     Int ::
//     Int ::
//     Int ::
//     Int ::
//     Int ::
//     Int ::
//     Int ::
//     Int ::
// //
//     Int ::
//     Int ::
//     Int ::
//     Int ::
//     Int ::
//     Int ::
//     Int ::
//     Int ::
//     Int ::
//     Int ::
// //
//     Int ::
//     Int ::
//     Int ::
//     Int ::
//     Int ::
//     Int ::
//     Int ::
//     Int ::
//     Int ::
//     Int ::
// //
//     Int ::
//     Int ::
//     Int ::
//     Int ::
//     Int ::
//     Int ::
//     Int ::
//     Int ::
//     Int ::
//     Int ::
// //
//     Int ::
//     Int ::
//     Int ::
//     Int ::
//     Int ::
//     Int ::
//     Int ::
//     Int ::
//     Int ::
//     Int ::
// //
//     Int ::
//     Int ::
//     Int ::
//     Int ::
//     Int ::
//     Int ::
//     Int ::
//     Int ::
//     Int ::
//     Int ::
// //
//     Int ::
//     Int ::
//     Int ::
//     Int ::
//     Int ::
//     Int ::
//     Int ::
//     Int ::
//     Int ::
//     Int ::
// //
//     Int ::
//     Int ::
//     Int ::
//     Int ::
//     Int ::
//     Int ::
//     Int ::
//     Int ::
//     Int ::
//     Int ::
// //
    Boolean ::
    HNil
}
