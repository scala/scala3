//> using options -source:future

trait TypedArray[T, Repr]

trait Ops[T <: TypedArray[?, T]] {
  def typedArray(): T
}

object Test {
  def test1(ops: Ops[? <: TypedArray[?, ?]]) = ops.typedArray()
  def test2(ops: Ops[? <: TypedArray[? <: AnyRef, ?]]) = ops.typedArray() // ok, was error: Recursion limit exceeded.
}