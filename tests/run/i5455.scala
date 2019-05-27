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

  implicit class NatOps(val self: Nat) extends AnyVal {
    def *(other: Nat): Nat = self * other
    def toInt: Int = self.asInstanceOf
  }
}

object Test extends App {
  import Library._

  val x = Nat(3)
  val y = Nat(4)

  val a = x * y
  val b = double1(x)
  val c = double2(x)

  assert(a.toInt == 12)
  assert(b.toInt == 6)
  assert(c.toInt == 6)

  def double1(n: Nat): Nat = n * Nat(2)
  def double2(n: Nat): Nat = NatOps(n) * Nat(2)
}