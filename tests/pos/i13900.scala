import compiletime.ops.int.Max
import scala.annotation.targetName
opaque type Inlined[T] = T
object Inlined:
  extension [T](inlined: Inlined[T]) def value: T = inlined
  inline given fromValue[T <: Singleton]: Conversion[T, Inlined[T]] =
    value => value
  @targetName("fromValueWide")
  given fromValue[Wide]: Conversion[Wide, Inlined[Wide]] = value => value

  def forced[T](value: Any): Inlined[T] = value.asInstanceOf[T]
  extension [T <: Int](lhs: Inlined[T])
    def max[R <: Int](rhs: Inlined[R]) =
      forced[Max[T, R]](lhs.value max rhs.value)