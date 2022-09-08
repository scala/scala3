import scala.annotation.precise
import scala.annotation.unchecked.uncheckedVariance

object preciseLib:
  sealed trait Args
  sealed trait NoArgs extends Args
  sealed trait Args2[T1, T2] extends Args
  trait IRDFType
  trait IRDFVector extends IRDFType
  trait IRDFBool extends IRDFType
  sealed trait DFError
  class DFType[+T <: IRDFType, +A <: Args]
  type DFTypeAny = DFType[IRDFType, Args]
  object DFBool extends DFType[IRDFBool, NoArgs]
  type DFBool = DFBool.type
  type DFVector[+VT <: DFTypeAny, +VD <: NonEmptyTuple] =
    DFType[IRDFVector, Args2[VT @uncheckedVariance, VD @uncheckedVariance]]

  trait TC[@precise -TCT1]:
    type Type <: DFTypeAny
  object TC:
    transparent inline given ofDFType[TCT2 <: DFTypeAny]: TC[TCT2] = new TC[TCT2]:
      type Type = TCT2

  extension [@precise EXT, @precise EXD <: Int](t: EXT)(using tc: TC[EXT])
    def XX(
      cellDim: EXD
    ): DFVector[tc.Type, Tuple1[EXD]] = ???