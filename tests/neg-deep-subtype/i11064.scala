trait TypedArray[T, Repr]

trait Ops[T <: TypedArray[_, T]] {
  def typedArray(): T
}

object Test {
  def test(ops: Ops[_ <: TypedArray[_ <: AnyRef, _]]) = ops.typedArray()  // error: Recursion limit exceeded.
}