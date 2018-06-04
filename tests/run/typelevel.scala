object Test extends App {

  trait Nat

  case object Z extends Nat
  type Z = Z.type
  case class S[N <: Nat](n: Nat) extends Nat

  abstract class HasResult[T] { type Result = T }
  case class ToNat[+T](val value: T) extends HasResult[T]

  transparent def ToNat(inline n: Int): ToNat[Nat] =
    if n == 0 then new ToNat(Z)
    else {
      val n1 = ToNat(n - 1)
      new ToNat[S[n1.Result]](S(n1.value))
    }

  val x0 = ToNat(0)
  val y0: Z = x0.value
  val z0: x0.Result = y0
  val x1 = ToNat(1)
  val y1: S[Z] = x1.value
  val z1: x1.Result = y1
  val x2 = ToNat(2)
  val y2: S[S[Z]] = x2.value
  val z2: x2.Result = y2
  println(x0)
  println(x1)
  println(x2)

  trait HList {
    def isEmpty: Boolean
    def head: Any
    def tail: HList
  }

  class HNil extends HList {
    transparent override def isEmpty = true
    override def head: Nothing = ???
    override def tail: Nothing = ???
  }

  case object HNil extends HNil

  case class HCons[H, T <: HList](hd: H, tl: T) extends HList {
    transparent override def isEmpty = false
    override def head: H = this.hd
    override def tail: T = this.tl
  }

  transparent def concat(xs: HList, ys: HList): HList =
    if (xs.isEmpty) ys
    else HCons(xs.head, concat(xs.tail, ys))

  transparent val xs = HCons(1, HCons("a", HNil))

  val r0 = concat(HNil, HNil)
  val r1 = concat(HNil, xs)
  val r2 = concat(xs, HNil)
  val r3 = concat(xs, xs)

  val r4 = concat(HNil, HCons(1, HCons("a", HNil)))
  val r5 = concat(HCons(1, HCons("a", HNil)), HNil)
  val r6 = concat(HCons(1, HCons("a", HNil)), HCons(1, HCons("a", HNil)))
}