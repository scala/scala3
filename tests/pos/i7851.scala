trait Wrappable[T] { }
given Wrappable[Float] with { }

case class Wrapped[T: Wrappable](value: T)

trait Wrapper[T] { type WrappedT }
object Wrapper { type Aux[T <: Tuple, WrappedT0 <: Tuple] = Wrapper[T] { type WrappedT = WrappedT0 } }

given Wrapper[EmptyTuple] with { type WrappedT = EmptyTuple }

given [T: Wrappable]: Wrapper[T] with { type WrappedT = Wrapped[T] }

given [H: Wrappable, T <: Tuple, WrappedT0 <: Tuple](using Wrapper.Aux[T, WrappedT0]): Wrapper[H *: T] with {
  type WrappedT = Wrapped[H] *: WrappedT0
}

def wrappedFunction[F, FArgs <: Tuple, WrapperFArgs <: Tuple, R: Wrappable](
  function: F
)(input: FArgs)(using
  tf: util.TupledFunction[F, WrapperFArgs => Wrapped[R]],
  vs: Wrapper.Aux[FArgs, WrapperFArgs]
): (R, R => Option[FArgs]) = {
  val variableInput = input.asInstanceOf[WrapperFArgs] // This is not correct but it's ok for the sake of this example.
  val result = tf.tupled(function)(variableInput)
  return (result.value, (_: R) => None)
}

object WrapperTest {
  def test1(x: Wrapped[Float], y: Wrapped[Float], z: Wrapped[Float]): Wrapped[Float] = { x }
  val test2: (Wrapped[Float], Wrapped[Float], Wrapped[Float]) => Wrapped[Float] = { (x, y, z) => x }

  def main(args: Array[String]): Unit = {
    wrappedFunction(test1: ((Wrapped[Float], Wrapped[Float], Wrapped[Float]) => Wrapped[Float]))(5f, 11f, 3f)
    wrappedFunction(test1)(5f, 11f, 3f)
    wrappedFunction(test2)(5f, 11f, 3f)
  }
}
