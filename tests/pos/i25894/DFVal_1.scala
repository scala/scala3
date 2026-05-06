package dfhdl

final class DFVal[+T <: DFType, +M]

type DFValAny = DFVal[DFType, ModifierAny]
type DFConstOf[+T <: DFType] = DFVal[T, Modifier.CONST]
type DFValTP[+T <: DFType] = DFVal[T, Modifier]

inline def x = ${ ??? }

object DFVal:
  export DFXInt.Ops.{c1, c2}
  object Ops:
    type BoolOnlyOp
    type CarryOp
