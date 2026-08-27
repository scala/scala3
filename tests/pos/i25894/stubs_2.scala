package dfhdl
import DFVal.Ops.CarryOp
import DFVal.Ops.BoolOnlyOp

trait ExactOp2Aux[Op, C, O]

trait Modifier
type ModifierAny = Modifier
object Modifier:
  type CONST = Modifier

class DFType

class DFC(mutableDB: MutableDB)

object DFBoolOrBit:
  given bl[Op, O](using
      ExactOp2Aux[Op, DFC, O]
  ): ExactOp2Aux[BoolOnlyOp, DFC, O] = ???

object DFDecimal:
  object Ops:
    export DFXInt.Ops.*

object DFXInt:
  object Ops:
    type A = Int
    type B = String
    given arith1[Op <: A]: ExactOp2Aux[Op, DFC, DFValTP[DFType]] = ???
    given arith2[Op <: B]: ExactOp2Aux[Op, DFC, DFValTP[DFType]] = ???
    given c1: ExactOp2Aux[CarryOp, DFC, DFValTP[DFType]] = ???
    given c2: ExactOp2Aux[CarryOp, DFC, DFValTP[DFType]] = ???

type DFConstInt32 = DFConstOf[DFType]
