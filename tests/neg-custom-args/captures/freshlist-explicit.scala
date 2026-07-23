import caps.*

class Ref extends Mutable:
    var x = 0
    def get: Int = x
    update def put(y: Int): Unit = x = y

trait FreshList[+A] { this: FreshList[A]^ =>
  def isEmpty: Boolean
  consume def decons: FreshCons[A]^
}

class FreshCons[+A](consume val head: A^, consume val tail: FreshList[A]^) extends FreshList[A], Product {
  def isEmpty = false
  def get: this.type = this
  def canEqual(that: Any) = that.isInstanceOf[FreshCons[?]]
  def productArity: Int = 2

  def productElement(n: Int): Nothing = throw NotImplementedError()

  def _1: A^ = unsafe.unsafeAssumeSeparate(head)
  def _2: FreshList[A]^ = unsafe.unsafeAssumeSeparate(tail)
  consume def decons: FreshCons[A]^ = this
}

object FreshCons:
  def unapply[A](consume xs: FreshCons[A]^): FreshCons[A]^ = xs

object FreshNil extends FreshList[Nothing] {
  def isEmpty = true
  consume def decons = ???
}

def par[A, B](x: A^, y: B^): Unit = ()

def Test =
    val r1 = Ref()
    val xs0 = FreshNil
    val xs1 = FreshCons(r1, xs0)
    val xs = FreshCons(r1, xs1) // error separation
    val c = xs.decons
    val h = c.head
    par(h, c.head) // error separation


def Test4 =
    val xs: FreshList[Ref^{}]^ = FreshCons(Ref(), FreshCons(Ref(), FreshNil))
    val ys = xs match
      case FreshCons(h, t) => FreshCons(h, t)

    val zs = ys match
      case FreshCons(h, t) => FreshCons(h, t)

    val err = xs match // error separation
      case FreshCons(h, t) => FreshCons(h, t) // error separation // error separation
