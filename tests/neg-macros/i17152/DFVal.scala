package crash

trait TCConv[T <: DFTypeAny, V, O]:
  type Out <: O
  def conv(dfType: T, value: V): Out

class DFVal[+T <: DFTypeAny]
type DFValAny = DFVal[DFTypeAny]
type DFValOf[+T <: DFTypeAny] = DFVal[T]

object DFVal:
  trait TC[T <: DFTypeAny, R] extends TCConv[T, R, DFValAny]:
    type Out = DFValOf[T]
    final def apply(dfType: T, value: R): Out = ???

  object TC:
    export CompanionsDFBits.Val.TC.given
  end TC

  object Ops:
    extension [T <: DFTypeAny, A, C, I](dfVal: DFVal[T])
      def bits(using w: Width[T]): DFValOf[DFBits[w.Out]] = ???
    end extension
  end Ops
end DFVal