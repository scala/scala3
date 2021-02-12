// This test requires retyping from untyped trees after inlining, which is not supported anymore
object typelevel {
  case class Typed[T](value: T) { type Type = T }
}

trait Nat
case object Z extends Nat
case class S[N <: Nat](n: N) extends Nat

trait HList

// ()
case object HNil extends HList
// (H, T)

@annotation.showAsInfix(true)
case class HCons [H, T <: HList](hd: H, tl: T) extends HList

object Test extends App {
  import typelevel.*
  type HNil = HNil.type
  type Z = Z.type

  transparent inline def ToNat(inline n: Int): Typed[Nat] =
    if n == 0 then Typed(Z)
    else Typed(S(ToNat(n - 1).value))

  val x0 = ToNat(0)
  val y0: Z = x0.value
  val x1 = ToNat(1)
  val y1: S[Z] = x1.value
  val x2 = ToNat(2)
  val y2: S[S[Z]] = x2.value
  println(x0)
  println(x1)
  println(x2)

  transparent inline def toInt(n: Nat): Int = inline n match {
    case Z => 0
    case S(n1) => toInt(n1) + 1
  }

  inline val i0 = toInt(y0)
  val j0: 0 = i0
  inline val i2 = toInt(y2)
  val j2: 2 = i2

  transparent inline def concat(xs: HList, ys: HList): HList = inline xs match {
    case HNil => ys
    case HCons(x, xs1) => HCons(x, concat(xs1, ys))
  }

  val xs = HCons(1, HCons("a", HNil))

  val r0 = concat(HNil, HNil)
  val c0: HNil = r0
  val r1 = concat(HNil, xs)
  val c1: HCons[Int, HCons[String, HNil]] = r1
  val r2 = concat(xs, HNil)
  val c2: HCons[Int, HCons[String, HNil]] = r2
  val r3 = concat(xs, xs)
  val c3: HCons[Int, HCons[String, HCons[Int, HCons[String, HNil]]]] = r3

  val r4 = concat(HNil, HCons(1, HCons("a", HNil)))
  val c4: HCons[Int, HCons[String, HNil]] = r4
  val r5 = concat(HCons(1, HCons("a", HNil)) , HNil)
  val c5: HCons[Int, HCons[String, HNil]] = r5
  val r6 = concat(HCons(1, HCons("a", HNil)), HCons(true, HCons(1.0, HNil)))
  val c6: HCons[Int, HCons[String, HCons[Boolean, HCons[Double, HNil]]]] = r6

  transparent inline def nth(xs: HList, n: Int): Any = inline xs match {
    case HCons(x, _)   if n == 0 => x
    case HCons(_, xs1) if n > 0  => nth(xs1, n - 1)
  }

  val e0 = nth(r2, 0)
  val ce0: Int = e0
  val e1 = nth(r2, 1)
  val ce1: String = e1

  transparent inline def concatTyped(xs: HList, ys: HList): Typed[_ <: HList] = inline xs match {
    case HNil => Typed(ys)
    case HCons(x, xs1) => Typed(HCons(x, concatTyped(xs1, ys).value))
  }

  transparent def concatImpl(xs: HList, ys: HList): HList = xs match {
    case HNil => ys
    case HCons(x, xs1) => HCons(x, concatImpl(xs1, ys))
  }

  inline def concatErased(xs: HList, ys: HList): HList = {
    erased val r = concatTyped(xs, ys)
    concatImpl(xs, ys).asInstanceOf[r.Type]
  }

  {
    val r0 = concatErased(HNil, HNil)
    val c0: HNil = r0
    val r1 = concatErased(HNil, xs)
    val c1: HCons[Int, HCons[String, HNil]] = r1
    val r2 = concatErased(xs, HNil)
    val c2: HCons[Int, HCons[String, HNil]] = r2
    val r3 = concatErased(xs, xs)
    val c3: HCons[Int, HCons[String, HCons[Int, HCons[String, HNil]]]] = r3
  }


}