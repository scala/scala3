trait TypedArray[T, Repr]

trait Ops[T <: TypedArray[_, T]] {
  def typedArray(): T
}

object Test {
  def test(ops: Ops[_ <: TypedArray[_, _]]) = ops.typedArray()
}