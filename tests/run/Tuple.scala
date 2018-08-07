import annotation.showAsInfix

sealed trait Tuple

object Tuple {
  object Empty extends Tuple

  type Empty = Empty.type

  @showAsInfix
  final case class *: [H, T <: Tuple](hd: H, tl: T) extends Tuple

  class HListDeco(val xs: Tuple) extends AnyVal {
    transparent def *: [H] (x: H): Tuple = Tuple.*:.apply(x, xs)

    transparent def size: Int = Tuple.size(xs)
  }

  transparent def size(xs: Tuple): Int = xs match {
    case Empty => 0
    case _ *: xs1 => size(xs1) + 1
  }

  transparent implicit def hlistDeco(xs: Tuple): HListDeco = new HListDeco(xs)

  transparent def apply(): Tuple = Empty
  transparent def apply(x1: Any): Tuple = x1 *: Empty
  transparent def apply(x1: Any, x2: Any) = x1 *: x2 *: Empty

  val xs0 = Tuple()
  val xs1 = Tuple(2)
  val xs2 = Tuple(2, "a")
  val s0 = xs0.size
  val s1 = xs1.size
  val s2 = xs2.size
}

object Test extends App