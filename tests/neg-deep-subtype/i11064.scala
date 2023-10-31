trait TypedArray[T, Repr]

trait Ops[T <: TypedArray[?, T]] {
  def typedArray(): T
}

object Test {
  def test(ops: Ops[_ <: TypedArray[? <: AnyRef, ?]]) = ops.typedArray()  // error: Recursion limit exceeded.
}