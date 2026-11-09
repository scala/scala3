// https://github.com/scala/scala3/issues/26340
package dfhdl
import DFVal.Ops.CarryOp

trait ExactOp2Aux[Op]

object DFDecimal:
  object Ops:
    export DFXInt.Ops.*

object DFXInt:
  object Ops:
    given c1: ExactOp2Aux[CarryOp] = ???
