trait Nat {
  def toInt: Int
}

case object Z extends Nat {
  transparent def toInt = 0
}

case class S[N <: Nat](n: N) extends Nat {
  transparent def toInt = n.toInt + 1
}

class C {
  type ToNat = Int

  type ToNat2(n: Int) <: Nat =
    if n == 0 then Z.type
    else S[ToNat2(n - 1)]}

object Test extends C {

  override type ToNat(n: Int) <: Nat = // error: illegal override
    if n == 0 then Z.type
    else S[ToNat(n - 1)]

  override type ToNat2[X] = X    // error: illegal override

}