object A {

  object opaques {
    opaque type FlagSet = Long
    def FlagSet(bits: Long): FlagSet = bits
    def toBits(fs: FlagSet): Long = fs
  }
  val someFlag = FlagSet(1)
  type FlagSet = opaques.FlagSet
  def FlagSet(bits: Long): FlagSet = opaques.FlagSet(bits)

  extension (xs: FlagSet) {
    def bits: Long = opaques.toBits(xs)
    def | (ys: FlagSet): FlagSet = FlagSet(xs.bits | ys.bits)
  }
}

object B {
  type Variance = A.FlagSet

  val f: A.FlagSet = A.someFlag
  f.bits   // OK

  val v: Variance = A.someFlag
  v.bits // OK, used to fail with: value bits is not a member of B.Variance

  A.someFlag.bits  // OK
  var x = 0
  (if (x > 0) A.someFlag else A.someFlag).bits // OK, used to fail with: value bits is not a member of ?
}