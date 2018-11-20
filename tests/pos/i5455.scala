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

  val a = x * y               // inferred type is Library.Nat.Nat
  val b = double1(x)          // inferred type is Library.Nat
  val c = double2(x)          // inferred type is Library.Nat.Nat

  assert(a.toInt == 12)       // works
  //assert(b.toInt == 6)      // error: toInt is not a member of Library.Nat
  assert(c.toInt == 6)        // works

  def double0(n: Nat): Nat = n * Nat(2) // ok

  def double3(n: Nat): Nat = Nat.NatOps(n) * Nat(2) // ok


  def double1(n: Nat.Nat): Nat = n * Nat(2)     // output type is incorrect

  def double2(n: Nat.Nat): Nat.Nat =n * Nat(2)  // works
}