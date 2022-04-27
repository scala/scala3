class Value[T <: Int](val value: T)

sealed trait DFType:
  type Width <: Int
  val width: Value[Width]

object DFType:
  trait TC[T]:
    type Type <: DFType
    def apply(t: T): Type
  type Aux[T, Type0 <: DFType] = TC[T] { type Type = Type0 }
  transparent inline given ofDFType[T <: DFType]: TC[T] =
    new TC[T]:
      type Type = T
      def apply(t: T): Type = t

  extension [T, Type <: DFType](t: T)(using tc: Aux[T, Type])
    protected def getDFType: Type = tc(t)

final case class DFBits[W <: Int](width: Value[W]) extends DFType:
  type Width = W

val w: Value[8] = DFBits(Value[8](8)).getDFType.width // error