import caps.*

class Ref extends Mutable:
    var x = 0
    def get: Int = x
    update def put(y: Int): Unit = x = y

trait FreshList[+A] { this: FreshList[A]^ =>
  def isEmpty: Boolean
  consume def decons: FreshCons[A]^
}

case class FreshCons[+A](consume head: A^, consume tail: FreshList[A]^) extends FreshList[A] {
  def isEmpty = false
  consume def decons: FreshCons[A]^ = this
}

object FreshNil extends FreshList[Nothing] {
  def isEmpty = true
  consume def decons = ???
}

def par[A, B](x: A^, y: B^): Unit = ()

def Test =
    val r1 = Ref()
    val r2 = Ref()
    val xs0 = FreshNil
    val xs1 = FreshCons(r1, xs0)
    val xs = FreshCons(r2, xs1)
    val c = xs.decons
    val h = c.head
    val t = c.tail
    val xs2 = FreshCons(h, t)
    val xs3: FreshList[Ref^{}]^ = xs2

def Test2 =
    val xs = FreshCons(Ref(), FreshCons(Ref(), FreshNil))
    val c = xs.decons
    val (h, t) = (c.head, c.tail)
    val xs2 = FreshCons(h, t)
    val xs3: FreshList[Ref^{}]^ = xs2

def Test3 =
    val xs: FreshList[Ref^{}]^ = FreshCons(Ref(), FreshCons(Ref(), FreshNil))
    val FreshCons(h, t) = xs.decons
    val xs2 = FreshCons(h, t)
    val xs3: FreshList[Ref^{}]^ = xs2

def Test4 =
    val xs: FreshList[Ref^{}]^ = FreshCons(Ref(), FreshCons(Ref(), FreshNil))
    xs match
      case FreshCons(h, t) =>
        val xs2 = FreshCons(h, t)
        val xs3: FreshList[Ref^{}]^ = xs2
      case _ =>

def Test5 =
    val xs: FreshList[Ref^{}]^ = FreshCons(Ref(), FreshCons(Ref(), FreshNil))
    xs match
      case FreshCons(h, FreshCons(h2, t)) =>
        par(h, h2)
      case _ =>

def Test6 =
  val xs: FreshList[Ref^{}]^ = FreshCons(Ref(), FreshCons(Ref(), FreshNil))
  val c = xs.decons
  val h1 = c.head
  val h2 = c.tail.decons.head
  par(h1, h2)

