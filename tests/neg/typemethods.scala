trait Nat {
  def toInt: Int
}

case object Z extends Nat {
  transparent def toInt = 0
}

case class S[N <: Nat](n: N) extends Nat {
  transparent def toInt = n.toInt + 1
}

object Test extends App {

  type ToNat(n: Int) <: Nat =
    if n == 0 then Z.type
    else S[ToNat(n - 1)]

  type ToNat = Int  // error: double definition
}