object A {

  object opaques {
    opaque type FlagSet = Long
    def FlagSet(bits: Long): FlagSet = bits
  }
  //type FlagSet = opaques.FlagSet
  //def FlagSet(bits: Long): FlagSet = opaques.FlagSet(bits)
  export opaques.FlagSet

}
