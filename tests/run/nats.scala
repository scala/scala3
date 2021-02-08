abstract class Nat {
  def toInt: Int
}
object Nat {
  case class S[N <: Nat](pred: N) extends Nat {
    def toInt = pred.toInt + 1
  }
  class Z extends Nat {
    def toInt = 0
  }
  val Z = new Z
}

trait Plus[X <: Nat, Y <: Nat, R <: Nat] {
  def add(x: X, y: Y): R
}

object Test {
  import Nat.*
  implicit def zPlus[Y <: Nat]: Plus[Z, Y, Y] = new {
    def add(x: Z, y: Y): Y = y
  }
  implicit def sPlus[X <: Nat, Y <: Nat, R <: Nat](
      implicit ev: Plus[X, Y, R]
    ): Plus[S[X], Y, S[R]] = new {
    def add(x: S[X], y: Y): S[R] = S(ev.add(x.pred, y))
  }
  def plus[X <: Nat, Y <: Nat, R <: Nat](x: X, y: Y)(
      implicit ev: Plus[X, Y, R]
    ): R = ev.add(x, y)
  def main(args: Array[String]) = {
    val x = S(S(Z))
    val x1: S[S[Z]] = x
    val y = S(Z)
    val z = plus(x, y)
    val z1: S[S[S[Z]]] = z
    println(z.toInt)
  }
}

