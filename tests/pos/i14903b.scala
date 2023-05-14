import annotation.unchecked.uncheckedVariance

sealed trait HList
sealed trait HNil extends HList
case object HNil extends HNil
case class ::[+H, +T <: HList](head: H, tail: T) extends HList

type Concat[X <: HList, Y <: HList] <: HList = X match
  case HNil   => Y
  case h :: t => h :: Concat[t, Y]

/**
  * Decompose L into Prefix ++ Suffix if possible
*/
type StripSuffix[L <: HList, Suffix <: HList] <: Option[HList] = L match
  case Suffix => Some[HNil]
  case h :: t => StripSuffix[t, Suffix] match
    case Some[x] => Some[h :: x]
    case _ => None.type
  case _      => None.type

/**
  * type-level implementation of this logic:
  *   Out =
  *     R                      if T has a tail of type L
  *     (L dropRight T) ++ R   if L has a tail of type T
*/
sealed trait TailSwitch[L <: HList, T <: HList, R <: HList]:
  type Out <: HList

object TailSwitch:
  type TS[L <: HList, T <: HList, R <: HList] <: HList =
    StripSuffix[T, L] match
      case Some[_] => R
      case _ => StripSuffix[L, T] match
        case Some[x] => Concat[x, R]

  implicit def tailSwitch[L <: HList, T <: HList, R <: HList]: (TailSwitch[L, T, R] {
    type Out = TS[L, T, R]
  }) = new TailSwitch[L, T, R] { type Out = TS[L, T, R] }

/**
 * Rule popping I from stack and pushing back O
*/
sealed class Rule[-I <: HList, +O <: HList]:
  def ~[I2 <: HList, O2 <: HList](that: Rule[I2, O2])(implicit
      i: TailSwitch[I2, O @uncheckedVariance, I @uncheckedVariance],
      o: TailSwitch[O @uncheckedVariance, I2, O2]
  ): Rule[i.Out, o.Out] = ???

object Test:
  def dot = new Rule[HNil, HNil] {}
  def num = new Rule[HNil, Byte :: HNil] {}
  def pattern = num ~ dot ~ num ~ dot ~ num ~ dot ~ num // error
