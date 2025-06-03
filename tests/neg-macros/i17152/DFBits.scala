// nopos-error
package crash

import scala.quoted.*

class IRDFType
class IRDFBoolOrBit extends IRDFType
class IRDFDecimal extends IRDFType
class IRDFBits extends IRDFType

final class DFType[+T <: IRDFType, +A]
type DFTypeAny = DFType[IRDFType, Any]

trait Baz

trait Width[T]:
  type Out <: Int
object Width:
  given fromDFBoolOrBit: [T <: DFBoolOrBit] => Width[T]:
    type Out = 1
  transparent inline given [T]: Width[T] = ${ getWidthMacro[T] }
  def getWidthMacro[T](using Quotes, Type[T]): Expr[Width[T]] =
    '{
      new Width[T]:
        type Out = 1
    }
end Width

extension [T](t: T)(using baz: Baz) def width: 1 = ???

trait Check[T1 <: Int, T2 <: Int]

type DFBits[W <: Int] = DFType[IRDFBits, Tuple1[W]]

private object CompanionsDFBits:
  object Val:
    trait Candidate[R]:
      type OutW <: Int
      def apply(value: R): DFValOf[DFBits[OutW]]
    object Candidate:
      given fromDFUInt: [W <: Int, R <: DFValOf[DFDecimal]] => Candidate[R]:
        type OutW = W
        def apply(value: R): DFValOf[DFBits[W]] =
          import DFVal.Ops.bits
          value.bits
          ???
    end Candidate

    object TC:
      import DFVal.TC
      given DFBitsFromCandidate[
          LW <: Int,
          V
      ](using candidate: Candidate[V])(using
          check: Check[LW, candidate.OutW]
      ): TC[DFBits[LW], V] with
        def conv(dfType: DFBits[LW], value: V): DFValOf[DFBits[LW]] =
          val dfVal = candidate(value)
          ???
    end TC
  end Val

end CompanionsDFBits

type DFBoolOrBit = DFType[IRDFBoolOrBit, Any]
type DFDecimal = DFType[IRDFDecimal, Any]
object DFDecimal:
  def foo(arg1: Int, arg2: Int): Unit = ???

  object Val:
    object TC:
      import DFVal.TC
      given [R]: TC[DFDecimal, R] = ???
      def apply(
          dfType: DFDecimal,
          dfVal: DFValOf[DFDecimal]
      ): DFValOf[DFDecimal] =
        foo(dfType.width, dfVal.width)
        dfVal
    end TC
  end Val
end DFDecimal