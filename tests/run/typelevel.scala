object Test extends App {

  trait Nat

  case object Z extends Nat
  type Z = Z.type
  case class S[N <: Nat](n: Nat) extends Nat

  abstract class HasResult[T] { type Result = T }
  case class ToNat[+T](val value: T) extends HasResult[T]

  transparent def ToNat(inline n: Int): ToNat[Nat] =
    if n == 0 then new ToNat(Z)
    else {
      val n1 = ToNat(n - 1)
      new ToNat[S[n1.Result]](S(n1.value))
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
}