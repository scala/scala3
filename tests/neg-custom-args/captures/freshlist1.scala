import caps.*

class Ref extends Mutable:
    var x = 0
    def get: Int = x
    update def put(y: Int): Unit = x = y

trait FreshList[+A] { this: FreshList[A]^ =>
  def isEmpty: Boolean
  consume def decons: FreshCons[A]^
}

case class FreshCons[+A](consume val head: A^, val tail: FreshList[A]^) extends FreshList[A] {
  def isEmpty = false
  consume def decons: FreshCons[A]^ = this
}

object FreshNil extends FreshList[Nothing] {
  def isEmpty = true
  consume def decons = ???
}

def par[A, B](x: A^, y: B^): Unit = ()


def Test =
    val xs1 = FreshCons(Ref(), FreshNil)
    val h1 = xs1._1
    val h2 = xs1._1
    par(h1, h2)    // error // error

def Test1 =
    val xs2 = FreshCons(Ref(), FreshNil)
    val h3 = xs2.productElement(0)
    val h4 = xs2.productElement(1)
    par(h3, h4)    // no error, since productElement returns Any

def Test2 =
    val r1: Ref^ = Ref()
    val xs0 = FreshNil
    val xs1 = new FreshCons(r1, xs0)
    val ys1 = xs1.decons // ok
    val ys2 = xs1.decons // error
    par(ys1, ys2)
