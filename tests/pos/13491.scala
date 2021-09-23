import scala.annotation.unchecked.uncheckedVariance

import scala.language.implicitConversions

sealed trait HList extends Product with Serializable
final case class ::[+H, +T <: HList](head: H, tail: T) extends HList

sealed trait HNil extends HList
case object HNil extends HNil

trait HListable[T] {
  type Out <: HList
}

object HListable {
  type HL0[T] <: HList = T match {
    case Unit     => HNil
    case HNil     => HNil
    case ::[a, b] => ::[a, b]
    case _        => T :: HNil
  }

  implicit def calc[T]: HListable[T] { type Out = HL0[T] } = ???
}

sealed trait TailSwitch[L <: HList, T <: HList, R <: HList] {
  type Out <: HList
}
object TailSwitch {
  type Reverse0[Acc <: HList, L <: HList] <: HList = L match {
    case HNil     => Acc
    case ::[h, t] => Reverse0[h :: Acc, t]
  }

  type Reverse1[L <: HList] <: HList = L match {
    case HNil     => HNil
    case ::[h, t] => Reverse0[h :: HNil, t]
  }

  type Prepend0[A <: HList, B <: HList] <: HList = A match {
    case HNil     => B
    case ::[h, t] => ::[h, Prepend0[t, B]]
  }

  // type-level implementation of this algorithm:
  //   @tailrec def rec(L, LI, T, TI, R, RI) =
  //     if (TI <: L) R
  //     else if (LI <: T) RI.reverse ::: R
  //     else if (LI <: HNil) rec(L, HNil, T, TI.tail, R, RI)
  //     else if (TI <: HNil) rec(L, LI.tail, T, HNil, R, LI.head :: RI)
  //     else rec(L, LI.tail, T, TI.tail, R, LI.head :: RI)
  //   rec(L, L, T, T, R, HNil)
  type TailSwitch0[L <: HList, LI <: HList, T <: HList, TI <: HList, R <: HList, RI <: HList] <: HList = TI match {
    case L => R
    case _ =>
    LI match {
      case T => Prepend0[Reverse1[RI], R]
      case HNil =>
      TI match {
        case ::[_, t] => TailSwitch0[L, HNil, T, t, R, RI]
      }
      case ::[h, t] =>
      TI match {
        case HNil      => TailSwitch0[L, t, T, HNil, R, h :: RI]
        case ::[_, tt] => TailSwitch0[L, t, T, tt, R, h :: RI]
      }
    }
  }

  type Aux[L <: HList, LI <: HList, T <: HList, TI <: HList, R <: HList, RI <: HList, Out <: HList] =
    TailSwitch[L, T, R] { type Out = TailSwitch0[L, L, T, T, R, HNil] }

  implicit def tailSwitch[L <: HList, T <: HList, R <: HList]
  : TailSwitch[L, T, R] { type Out = TailSwitch0[L, L, T, T, R, HNil] } = ???
}

sealed class Rule[-I <: HList, +O <: HList] {
  def ~[I2 <: HList, O2 <: HList](that: Rule[I2, O2])(implicit
                                                      i: TailSwitch[I2, O@uncheckedVariance, I@uncheckedVariance],
                                                      o: TailSwitch[O@uncheckedVariance, I2, O2]
  ): Rule[i.Out, o.Out] = ???

}
object Rule {
  type Rule0 = Rule[HNil, HNil]
  type RuleN[+L <: HList]   = Rule[HNil, L]

  def rule[I <: HList, O <: HList](r: Rule[I, O]): Rule[I, O] = ???
  implicit def valueMap[T](m: Map[String, T])(implicit h: HListable[T]): RuleN[h.Out] = ???
}

object Test {
  import Rule._
  val colors: Map[String, Int] = Map("red" -> 1, "green" -> 2, "blue" -> 3)
  def EOI: Rule0= ???
  val r = rule(colors ~ EOI)
}
