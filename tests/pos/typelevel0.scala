
trait HList {
  def length: Int = ???
  def head: Any
  def tail: HList

  inline def isEmpty <: Boolean = length == 0
}

case object HNil extends HList {
  inline override def length <: Int = 0
  def head: Nothing = ???
  def tail: Nothing = ???
}

case class :: [+H, +T <: HList] (hd: H, tl: T) extends HList {
  inline override def length <: Int = 1 + tl.length
  def head: H = this.hd
  def tail: T = this.tl
}

object Test extends App {
  type HNil = HNil.type
  val xs = new ::(1, new ::("a", HNil))
  inline val y = xs.length
  inline val ise = xs.isEmpty
  val hd = xs.head
  val tl = xs.tail
  val tl2 = xs.tail.tail

  type Concat[Xs <: HList, Ys <: HList] <: HList = Xs match {
    case HNil => Ys
    case x1 :: xs1 => x1 :: Concat[xs1, Ys]
  }

  def concat[Xs <: HList, Ys <: HList](xs: Xs, ys: Ys): Concat[Xs, Ys] = {
    if xs.isEmpty then ys
    else ::(xs.head, concat(xs.tail, ys))
  }.asInstanceOf

  val xs0 = concat(HNil, xs)
  val xs1 = concat(xs, HNil)
  val xs2 = concat(xs, xs)
  val e2: Int = xs2.tail.tail.head
}