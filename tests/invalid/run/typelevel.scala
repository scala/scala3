
trait Nat {
  def toInt: Int = ???
}

case object Z extends Nat {
  inline override def toInt = 0
}

case class S[N <: Nat](n: N) extends Nat {
  inline override def toInt = n.toInt + 1
}

trait HList {
  def length: Int = ???
  def head: Any
  def tail: HList
  inline def isEmpty: Boolean =
    length == 0
}

// ()
case object HNil extends HList {
  inline override def length = 0
  def head: Nothing = ???
  def tail: Nothing = ???
}

// (H, T)
@annotation.showAsInfix(true)
case class HCons [H, T <: HList](hd: H, tl: T) extends HList {
  inline override def length = 1 + tl.length
  def head: H = this.hd
  def tail: T = this.tl
}

case class ToNat[T](val value: T) {
  type Result = T
}

object Test extends App {
  type HNil = HNil.type
  type Z = Z.type

  inline def ToNat(inline n: Int): ToNat[Nat] =
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
  inline val i0 = y0.toInt
  val j0: 0 = i0
  inline val i2 = y2.toInt
  val j2: 2 = i2

  class HListDeco(private val as: HList) extends AnyVal {
    inline def :: [H] (a: H) = HCons(a, as)
    inline def ++ (bs: HList) = concat(as, bs)
  }
  inline def concat(xs: HList, ys: HList): HList =
    if xs.isEmpty then ys
    else HCons(xs.head, concat(xs.tail, ys))

  val xs = HCons(1, HCons("a", HNil))

  val r0 = concat(HNil, HNil)
  val r1 = concat(HNil, xs)
  val r2 = concat(xs, HNil)
  val r3 = concat(xs, xs)

  val r4 = concat(HNil, HCons(1, HCons("a", HNil)))
  val r5 = concat(HCons(1, HCons("a", HNil)) , HNil)
  val r6 = concat(HCons(1, HCons("a", HNil)), HCons(true, HCons(1.0, HNil)))

  inline def size(xs: HList): Nat =
    if xs.isEmpty then Z
    else S(size(xs.tail))

  inline def sizeDefensive(xs: HList): Nat = inline xs.isEmpty match {
    case true => Z
    case false => S(sizeDefensive(xs.tail))
  }

  val s0 = size(HNil)
  val s1 = size(xs)
  inline val l0 = HNil.length
  val l0a: 0 = l0
  inline val l1 = xs.length
  val l1a: 2 = l1

  inline def index(xs: HList, inline idx: Int): Any =
    if idx == 0 then xs.head
    else index(xs.tail, idx - 1)

  val s2 = index(xs, 0)
  val ss2: Int = s2
  val s3 = index(xs, 1)
  var ss3: String = s3
  def s4 = index(xs, 2)
  def ss4: Nothing = s4
  val s5 = index(xs, xs.length - 1)
  val ss5: String = s5


  //val ys = 1 :: "a" :: HNil

  inline implicit def hlistDeco(xs: HList): HListDeco = new HListDeco(xs)

  val rr0 = new HListDeco(HNil).++(HNil)
  val rr1 = HNil ++ xs
  val rr2 = xs ++ HNil
  val rr3 = xs ++ xs
  val rr3a: HCons[Int, HCons[String, HCons[Int, HCons[String, HNil]]]] = rr3

  inline def f(c: Boolean): Nat = {
    def g[X <: Nat](x: X): X = x
    g(if (c) Z else S(Z))
  }

  val f1: Z = f(true)
  val f2: S[Z] = f(false)

  inline def mapHead[T, R](t: T)(implicit fh: T => R): R = fh(t)
  inline def map(xs: HList): HList = {

    if (xs.isEmpty) HNil
    else HCons(mapHead(xs.head), map(xs.tail))
  }

  implicit def mapInt: Int => Boolean = (i: Int) => i < 23
  implicit val mapString: String => Int = (s: String) => s.length
  implicit val mapBoolean: Boolean => String = (b: Boolean) => if(b) "yes" else "no"

  val res = map(HCons(23, HCons("foo", HCons(true, HNil))))
  val res1: Boolean `HCons` (Int `HCons` (String `HCons` HNil)) = res

/*
  inline def toInt1[T]: Int = type T match {
    case Z => 0
    case S[type N] => toInt[N] + 1
  }

  inline def toInt1[T]: Nat = implied match {
    case C[type T, type U], T =:= U =>
    case T <:< S[type N] => toInt[N] + 1
  }
*/
}