class Arr[T](private val underlying: scala.Array[T]) extends AnyVal

abstract class SeqMonoTransforms[+A, +Repr] {
  protected[this] def fromIterableWithSameElemType(): Repr
  def getFIWSET: Repr = fromIterableWithSameElemType()
}

class ArrOps[A](val xs: Arr[A]) extends SeqMonoTransforms[A, Arr[A]] {
  def fromIterableWithSameElemType(): Arr[A] = xs // error: bridge clashes with member
}

object Test {
  def main(args: Array[String]) = {
    val t = new ArrOps(new Arr(Array(1, 2, 3)))
    val t2 = t.getFIWSET
  }
}
