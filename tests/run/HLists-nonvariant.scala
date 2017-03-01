sealed trait HList
case class HCons[HD, TL](hd: HD, tl: TL) extends HList
case object HNil extends HList

sealed trait Num
case object Zero extends Num
case class Succ[N <: Num](pred: N) extends Num

object Test {

  type HNil = HNil.type
  type Zero = Zero.type

  trait Concat[Xs <: HList, Ys <: HList, Zs <: HList] {
    def concat(xs: Xs, ys: Ys): Zs
  }

  implicit def concatNil[Xs <: HList]: Concat[HNil, Xs, Xs] =
    new Concat[HNil, Xs, Xs] {
      def concat(fst: HNil, snd: Xs) = snd
    }

  implicit def concatCons[X, Xs <: HList, Ys <: HList, Zs <: HList](
    implicit ev: Concat[Xs, Ys, Zs]
  ): Concat[HCons[X, Xs], Ys, HCons[X, Zs]] =
    new Concat[HCons[X, Xs], Ys, HCons[X, Zs]] {
      def concat(xs: HCons[X, Xs], ys: Ys): HCons[X, Zs] =
        HCons(xs.hd, ev.concat(xs.tl, ys))
    }

  def concat[Xs <: HList, Ys <: HList, Zs <: HList](xs: Xs, ys: Ys)(implicit ev: Concat[Xs, Ys, Zs]): Zs =
    ev.concat(xs, ys)

  val xs = HCons(1, HCons("A", HNil))
  val ys = HCons(true, HNil)
  val zs = concat(xs, ys)
  val zs1: HCons[Int, HCons[String, HCons[Boolean, HNil]]] = zs

  trait At[Xs <: HList, N <: Num] {
    type Out
    def at(xs: Xs, n: N): Out
  }

  implicit def atZero[XZ, Xs <: HList]: At[HCons[XZ, Xs], Zero] { type Out = XZ } =
    new At[HCons[XZ, Xs], Zero] {
      type Out = XZ
      def at(xs: HCons[XZ, Xs], n: Zero) = xs.hd
    }

  implicit def atSucc[XX, Xs <: HList, N <: Num](
    implicit ev: At[Xs, N]
  ): At[HCons[XX, Xs], Succ[N]] { type Out = ev.Out } = new At[HCons[XX, Xs], Succ[N]] {
    type Out = ev.Out
    def at(xs: HCons[XX, Xs], n: Succ[N]): Out = ev.at(xs.tl, n.pred)
  }

  def at[Xs <: HList, N <: Num](xs: Xs, n: N)(
    implicit ev: At[Xs, N]
  ): ev.Out = ev.at(xs, n)

  def main(args: Array[String]) = {
    val ys1 = HCons(1, HNil)
    println(at(ys1, Zero))

    val ys2 = HCons(1, HCons("A", HNil))
    val y2 = at(ys2, Succ(Zero))
    println(at(ys2, Succ(Zero)))
    val ys3 = HCons(1, HCons("A", HCons(true, HNil)))
    println(at(ys3, Succ(Succ(Zero))))
    val ys4 = HCons(1, HCons("A", HCons(true, HCons(1.0, HNil))))
    println(at(ys4, Succ(Succ(Zero))))

    println(zs1)
    println(at(zs1, Zero))
    println(at(zs1, Succ(Zero)))
    println(at(zs1, Succ(Succ(Zero))))
  }
}
