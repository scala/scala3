class Arr[T](val underlying: scala.Array[T]) extends AnyVal {
  def foo = underlying
}

abstract class SeqMonoTransforms[+A, +Repr] {
  protected[this] def fromIterableWithSameElemType(): Repr
  def getFIWSET: Repr = fromIterableWithSameElemType
}

class ArrOps[A](val xs: Arr[A]) extends SeqMonoTransforms[A, Arr[A]] {
  def fromIterableWithSameElemType(): Arr[A] = xs
}

object Test {
  def main(args: Array[String]) = {
    val arr = new Arr(Array(1, 2, 3))
    val t = new ArrOps(arr)
    val t2 = t.getFIWSET
    val x = arr.foo
  }
}
