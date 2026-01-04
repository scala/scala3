import caps.*

class Ref[T](init: T) extends Mutable:
  private var cur: T = init
  def get(): T = cur
  update def set(x: T): Unit = cur = x

object Test:

  class LL

  def filterImpl(ll: LL^): LL^{ll} =
    var x: LL^{ll} = ???
    val cl: () ->{x, x.rd, ll} LL^{ll} = ???
    val nll: LL^{cl} = ???
    nll // error

  def filterImpl1(ll: LL^) =
    val x: Ref[LL^{ll}] = ???
    val cl: () ->{x, x.rd, ll} LL^{ll} = ???
    val nll: LL^{cl} = ???
    nll

  val x1 = filterImpl1(LL()) // error
  val _: LL^{cap.rd}= x1
  val _: LL = x1 // error

  def filterImpl2(ll: LL^): LL^{ll} =
    val x: Ref[LL^{ll}] = ???
    val cl: () ->{x, x.rd, ll} LL^{ll} = ???
    val nll: LL^{cl} = ???
    nll // error
