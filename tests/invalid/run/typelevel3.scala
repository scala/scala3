// This test requires retyping from untyped trees after inlining, which is not supported anymore
trait HList {
  def length: Int = 0
  def head: Any
  def tail: HList

  inline def isEmpty: Boolean = length == 0
}

case object HNil extends HList {
  inline override def length = 0
  def head: Nothing = ???
  def tail: Nothing = ???
}

case class HCons[H, T <: HList](hd: H, tl: T) extends HList {
  inline override def length = 1 + tl.length
  def head: H = this.hd
  def tail: T = this.tl
}

case class Typed[T](val value: T) { type Type = T }

object Test extends App {
  type HNil = HNil.type

  inline def concat(xs: HList, ys: HList): Typed[_ <: HList] =
    if xs.isEmpty then Typed(ys)
    else Typed(HCons(xs.head, concat(xs.tail, ys).value))

  val xs = HCons(1, HCons("a", HCons("b", HNil)))
  val ys = HCons(true, HCons(1.0, HNil))
  val zs = concat(xs, ys)
  val zs1: zs.Type = zs.value

  val control: HCons[Int, HCons[String, HCons[String, HCons[Boolean, HCons[Double, HNil]]]]] = zs.value

  inline def index(xs: HList, idx: Int): Typed[_] =
    if idx == 0 then Typed(xs.head)
    else Typed(index(xs.tail, idx - 1).value)

  val zsv = zs.value
  val zs0 = index(zsv, 0)
  val zs0c: Int = zs0.value
  val zs4 = index(zsv, 4)
  val zs4c: Double = zs4.value
  def zs5 = index(zsv, 5)
  def zs5c: Nothing = zs5.value

  def opaqueConcat(xs: HList, ys: HList): HList =
    if xs.isEmpty then ys
    else HCons(xs.head, opaqueConcat(xs.tail, ys))

  inline def compactConcat(xs: HList, ys: HList): HList = {
    erased val r = concat(xs, ys)
    opaqueConcat(xs, ys).asInstanceOf[r.Type]
  }

  val czs = compactConcat(xs, ys)
}