sealed trait Eq[A, B]
case class Same[C]() extends Eq[C, C]

sealed trait Tag[T]
case class AStr[S](witness: Eq[S, String]) extends Tag[S]
case class AInt[I](witness: Eq[I, Int])    extends Tag[I]

class Test:
  def t1(tag: Tag[Int]): Unit = tag match
    case AInt(Same()) => ()
