// nopos-error
trait DFBitsCompanion:
  type Token[W <: Int] = DFToken[DFType.DFBits[W]]
  extension [LW <: Int](lhs: DFType.DFBits.Token[LW]) def foo: Unit = lhs.width
