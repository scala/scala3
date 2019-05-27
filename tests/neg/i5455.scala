object Library {

  opaque type Nat = Int

  object Nat {
    def apply(n: Int): Nat = {
      require(n >= 0)
      n
    }
    def times(x: Nat, y: Nat): Nat = x * y
    def toInt(n: Nat): Int = n

  }
  implied NatOps {
    def (x: Nat) * (y: Nat): Nat = x * y
    def (x: Nat) toInt: Int = x
  }
}

object User extends App {
  import Library._

  val x = Nat(3)
  val y = Nat(4)

  val a = x * y
  val b = double1(x)
  val c = double2(x)

  def double0(n: Nat): Nat = n * Nat(2) // ok

  def double1(n: Nat.Nat): Nat = n * Nat(2)     // error

  def double2(n: Nat.Nat): Nat.Nat = n * Nat(2)  // error // error
}