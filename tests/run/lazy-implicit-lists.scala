trait L
case class C(hd: Int, tl: L) extends L
case object N extends L

trait Sum[+S1, +S2]
case class Fst[+F](x: F) extends Sum[F, Nothing]
case class Snd[+S](x: S) extends Sum[Nothing, S]

case class Prod[+P1, +P2](fst: P1, snd: P2)

trait shaped[SH1, SH2] {
  def toShape(x: SH1): SH2
  def fromShape(x: SH2): SH1
}

object Test {

  type LShape = Sum[Prod[Int, L], Unit]

  implicit def LShape: L `shaped` LShape =
    new (L `shaped` LShape) {
      def toShape(xs: L) = xs match {
        case C(x, xs1) => Fst(Prod(x, xs1))
        case N => Snd(())
      }
      def fromShape(sh: LShape) = sh match {
        case Fst(Prod(x, xs1)) => C(x, xs1)
        case Snd(()) => N
      }
    }

  trait Listable[T] {
    def toList(x: T): List[Int]
  }

  implicit def ShapedListable[T, U](implicit
    ev1: T shaped U,
    ev2: Listable[U]
  ): Listable[T] =
    new Listable[T] {
      def toList(x: T) = ev2.toList(ev1.toShape(x))
    }

  implicit def SumListable[T, U](implicit
    ev1: => Listable[T],
    ev2: => Listable[U]
  ): Listable[Sum[T, U]] =
    new Listable[Sum[T, U]] {
      def toList(s: Sum[T, U]) = s match {
        case Fst(x) => ev1.toList(x)
        case Snd(x) => ev2.toList(x)
      }
    }

  implicit def ProdListable[T, U](implicit
    ev1: Listable[T],
    ev2: Listable[U]
  ): Listable[Prod[T, U]] =
    new Listable[Prod[T, U]] {
      def toList(p: Prod[T, U]) = ev1.toList(p.fst) ++ ev2.toList(p.snd)
    }

  implicit def IntListable: Listable[Int] =
    new Listable[Int] {
      def toList(n: Int) = n :: Nil
    }


  implicit def UnitListable: Listable[Unit] =
    new Listable[Unit] {
      def toList(u: Unit) = Nil
    }

  def toList[T, U >: T](x: T)(implicit ev1: Listable[U]) = ev1.toList(x)

  def main(args: Array[String]) = {
    locally { // with specialized Listable
      implicit lazy val LListable: Listable[L] = ShapedListable
      println(toList(N))
      println(toList(C(1, C(2, C(3, N)))))
    }
    locally { // without specialized Listable
      println(toList(N))
      println(toList(C(1, C(2, C(3, N)))))
    }
  }
}
