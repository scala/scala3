import caps.*

class Ref extends Mutable:
    var x = 0
    def get: Int = x
    update def put(y: Int): Unit = x = y

trait FreshList[+A <: Any^{}] { this: FreshList[A]^ =>
  def isEmpty: Boolean
  consume def decons: FreshCons[A]^
}

class FreshCons[+A <: Any^{}](val head: A^, val tail: FreshList[A]^) extends FreshList[A] {
  def isEmpty = false
  consume def decons: FreshCons[A]^ = this
}

object FreshNil extends FreshList[Nothing] {
  def isEmpty = true
  consume def decons = ???
}

def Test =
  val r1: Ref^ = Ref()
  val r2: Ref^ = Ref()
  val xs0 = FreshNil
  val xs1 = FreshCons(r1, xs0)
  val xs = FreshCons(r2, xs1)
  val c = xs.decons
  val _: FreshCons[Ref^{}]{val head: Ref^; val tail: FreshList[Ref^{}]^}^ = c
  val h = c.head
  val t = c.tail
  val xs2 = FreshCons(h, t)
  val xs3: FreshList[Ref^{}]{val head: Ref^{h}; val tail: FreshList[Ref^{}]^{t}}^{h, t} = xs2
