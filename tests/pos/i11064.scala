//> using options -source:3.8

trait TypedArray[T, Repr]

trait Ops[T <: TypedArray[_, T]] {
  def typedArray(): T
}

object Test {
  def test1(ops: Ops[_ <: TypedArray[_, _]]) = ops.typedArray()
  def test2(ops: Ops[_ <: TypedArray[_ <: AnyRef, _]]) = ops.typedArray() // ok, was error: Recursion limit exceeded.
}