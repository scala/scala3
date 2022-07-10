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

class Tensor[T <: DType](dtype: T):
  def toSeq: Seq[ScalaType[T]] = Seq()
  def toArray: Array[ScalaType[T]] = Array() // error

@main
def Test =
  val t = Tensor(Float32) // Tensor[Float32]
  println(t.toSeq.headOption) // works, Seq[Float]
  println(t.toArray.headOption) // ClassCastException
