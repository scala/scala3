sealed abstract class DType
sealed class Float16 extends DType
sealed class Float32 extends DType
sealed class Int32 extends DType

object Float16 extends Float16
object Float32 extends Float32
object Int32 extends Int32

type ScalaType[U <: DType] <: Int | Float = U match
  case Float16 => Float
  case Float32 => Float
  case Int32 => Int

abstract class Tensor[T <: DType]:
  def toArray: Array[ScalaType[T]]

object FloatTensor extends Tensor[Float16]:
  def toArray: Array[Float] = Array(1, 2, 3)

@main
def Test =
  val t = FloatTensor: Tensor[Float16] // Tensor[Float32]
  println(t.toArray.headOption) // was ClassCastException
