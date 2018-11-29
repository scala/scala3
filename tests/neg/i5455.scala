object Library {

  opaque type Nat = Int

  object Nat {
    def apply(n: Int): Nat = {
      require(n >= 0)
      n
    }
    def times(x: Nat, y: Nat): Nat = x * y
    def toInt(n: Nat): Int = n

    implicit class NatOps(val self: Nat) extends AnyVal {
      def *(other: Nat): Nat = self * other
      def toInt: Int = self.asInstanceOf
    }
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