trait Nat
case class S(x: Nat) extends Nat
case class Z() extends Nat

trait Sum[+S1, +S2]
case class Fst[+F](x: F) extends Sum[F, Nothing]
case class Snd[+S](x: S) extends Sum[Nothing, S]

trait shaped[SH1, SH2] {
  def toShape(x: SH1): SH2
  def fromShape(x: SH2): SH1
}

object Test {

  type NatShape = Sum[Nat, Z]

  implicit def natShape: Nat `shaped` NatShape =
    new (Nat `shaped` NatShape) {
      def toShape(n: Nat) = n match {
        case S(m) => Fst(m)
        case Z() => Snd(Z())
      }
      def fromShape(s: NatShape) = s match {
        case Fst(n) => S(n)
        case Snd(_) => Z()
      }
    }

  trait Countable[T] {
    def count(x: T): Int
  }

  implicit def ShapedCountable[T, U](implicit
    ev1: T shaped U,
    ev2: Countable[U]
  ): Countable[T] =
    new Countable[T] {
      def count(x: T) = ev2.count(ev1.toShape(x))
    }

  implicit def SumCountable[T, U](implicit
    ev1: => Countable[T]
  ): Countable[Sum[T, U]] =
    new Countable[Sum[T, U]] {
      def count(s: Sum[T, U]) = s match {
        case Fst(x) => ev1.count(x) + 1
        case Snd(_) => 0
      }
    }

  def count[T, U >: T](x: T)(implicit ev1: Countable[U]) = ev1.count(x)

  def main(args: Array[String]) = {
    println(
      count(S(S(S(Z())))))
  }
}
