import annotation.showAsInfix

object typelevel {
  erased def erasedValue[T]: T = ???
  class Typed[T](val value: T) { type Type = T }
}

sealed trait Tuple
object Empty extends Tuple

@showAsInfix
final case class *: [H, T <: Tuple](hd: H, tl: T) extends Tuple

object Tuple {
  import typelevel._
  type Empty = Empty.type

  class TupleOps(val xs: Tuple) extends AnyVal {
    transparent def *: [H] (x: H): Tuple = new *:(x, xs)
    transparent def size: Int = xs match {
      case Empty => 0
      case _ *: xs1 => xs1.size + 1
    }
    transparent def apply(n: Int): Any = xs match {
      case x *: _   if n == 0 => x
      case _ *: xs1 if n > 0  => xs1.apply(n - 1)
    }
    transparent def **: (ys: Tuple): Tuple = ys match {
      case Empty    => xs
      case y *: ys1 => y *: (ys1 **: xs)
    }
    transparent def head = xs match {
      case x *: _ => x
    }
    transparent def tail = xs match {
      case _ *: xs => xs
    }
  }

  val emptyArray = Array[Object]()

  transparent def toObj(t: Any) = t.asInstanceOf[Object]

  transparent def toArray(t: Tuple): Array[Object] = t.size match {
    case 0 => emptyArray
    case 1 => Array(toObj(t(0)))
    case 2 => Array(toObj(t(0)), toObj(t(1)))
    case 3 => Array(toObj(t(0)), toObj(t(1)), toObj(t(2)))
    case 4 => Array(toObj(t(0)), toObj(t(1)), toObj(t(2)), toObj(t(3)))
  }

  transparent implicit def tupleDeco(xs: Tuple): TupleOps = new TupleOps(xs)

  transparent def apply(): Tuple = Empty
  transparent def apply(x1: Any): Tuple = x1 *: Empty
  transparent def apply(x1: Any, x2: Any) = x1 *: x2 *: Empty
  transparent def apply(x1: Any, x2: Any, x3: Any) = x1 *: x2 *: x3 *: Empty

  val xs0 = Tuple()
  val xs1 = Tuple(2)
  val xs2 = Tuple(2, "a")
  val xs3 = Tuple(true, 1, 2.0)
  transparent val s0 = xs0.size; val s0c: 0 = s0
  transparent val s1 = xs1.size; val s1c: 1 = s1
  transparent val s2 = xs2.size; val s2c: 2 = s2
  transparent val s3 = xs3.size; val s3c: 3 = s3
  val e0 = xs3(0); val e0c: Boolean = e0
  val e1 = xs3(1); val e1c: Int = e1
  val e2 = xs3(2); val e2c: Double = e2

  val conc0 = xs0 **: xs3
  val conc1 = xs3 **: xs0
  val conc2 = xs2 **: xs3
  val e3c: Int = conc0(1)
  val e4c: Int = conc1(1)
  val e5c: Int = conc2(0)
  val e6c: Double = conc2(4)

}

object Test extends App