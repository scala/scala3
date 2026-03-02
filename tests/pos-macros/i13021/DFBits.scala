object DFBits:
  opaque type Token[W <: Int] <: DFToken.Of[Int] = DFToken.Of[Int]
  extension [W <: Int](token: Token[W])
    def data: Int =
      token.asIR
      1
