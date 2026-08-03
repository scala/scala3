// it's also OK if this errors due to a cycle, what matters is the compiler shouldn't crash
trait DFBitsCompanion:
  type Token[W <: Int] = DFToken[DFType.DFBits[W]]
  extension [LW <: Int](lhs: DFType.DFBits.Token[LW]) def foo: Unit = lhs.width
