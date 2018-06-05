object Test extends App {

  trait Nat {
    def toInt: Int
  }

  case object Z extends Nat {
    transparent def toInt = 0
  }

  type Z = Z.type
  case class S[N <: Nat](n: N) extends Nat {
    transparent def toInt = n.toInt + 1
  }

  abstract class HasResult[T] { type Result = T }
  case class ToNat[+T](val value: T) extends HasResult[T]

  transparent def ToNat(inline n: Int): ToNat[Nat] =
    if n == 0 then new ToNat(Z)
    else {
      val n1 = ToNat(n - 1)
      new ToNat(S(n1.value))
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
  transparent val i0 = y0.toInt
  val j0: 0 = i0
  transparent val i2 = y2.toInt
  val j2: 2 = i2

  trait HList {
    transparent def isEmpty: Boolean = length == 0
    def length: Int
    def head: Any
    def tail: HList
  }

  class HNil extends HList {
    transparent override def length = 0
    override def head: Nothing = ???
    override def tail: Nothing = ???
  }

  lazy val HNil: HNil = new HNil

  case class HCons[H, T <: HList](hd: H, tl: T) extends HList {
    transparent override def length = 1 + tl.length
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
  val r5 = concat(HCons(1, HCons("a", HNil)) , HNil)
  val r6 = concat(HCons(1, HCons("a", HNil)), HCons(1, HCons("a", HNil)))

  transparent def size(xs: HList): Nat =
    if (xs.isEmpty) Z
    else S(size(xs.tail))

  transparent def sizeDefensive(xs: HList): Nat = xs.isEmpty match {
    case true => Z
    case false => S(sizeDefensive(xs.tail))
  }
/*
  transparent def toInt1[T]: Nat = type T match {
    case Z => 0
    case S[type N] => toInt[N] + 1
  }

  transparent def toInt1[T]: Nat = implicit match {
    case T <:< Z => 0
    case T <:< S[type N] => toInt[N] + 1
  }
*/

  val s0 = size(HNil)
  val s1 = size(xs)
  transparent val l0 = HNil.length
  val l0a: 0 = l0
  transparent val l1 = xs.length
  val l1a: 2 = l1

  transparent def index(xs: HList, inline idx: Int): Any =
    if (idx == 0) xs.head
    else index(xs.tail, idx - 1)

  val s2 = index(xs, 0)
  val ss2: Int = s2
  val s3 = index(xs, 1)
  var ss3: String = s3
  def s4 = index(xs, 2)
  def ss4: Nothing = s4
  val s5 = index(xs, xs.length - 1)
  val ss5: String = s5

/** Does not work yet:

  implicit class HListDeco(transparent val xs: HList) {
    transparent def ++ (ys: HList) = concat(xs, ys)
  }

  val rr0 = HNil ++ HNil
  val rr1 = HNil ++ xs
  val rr2 = xs ++ HNil
  val rr3 = xs ++ xs
  val rr3a: HCons[Int, HCons[String, HCons[Int, HCons[String, HNil]]]] = rr3

*/
}