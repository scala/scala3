object A {

  object opaques {
    opaque type FlagSet = Long
    def FlagSet(bits: Long): FlagSet = bits.asInstanceOf // !!!
    def toBits(fs: FlagSet): Long = fs
  }
  val someFlag = FlagSet(1)
  type FlagSet = opaques.FlagSet
  def FlagSet(bits: Long): FlagSet = opaques.FlagSet(bits)

  delegate FlagOps {
    def (xs: FlagSet) bits: Long = opaques.toBits(xs)
  }
}

object B {
  type Variance = A.FlagSet

  val f: A.FlagSet = A.someFlag
  f.bits   // OK

  val v: Variance = A.someFlag
  v.bits // OK, used to fail with: value bits is not a member of B.Variance
}