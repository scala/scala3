import scala.reflect.ClassTag

class Arr[T](private val underlying: scala.Array[T]) extends AnyVal

trait SeqMonoTransforms[+A, +Repr] extends Any {
  protected[this] def fromIterableWithSameElemType(): Repr
}

class ArrOps[A](val xs: Arr[A]) extends AnyRef with SeqMonoTransforms[A, Arr[A]] {
  def fromIterableWithSameElemType(): Arr[A] = xs
}

object Test {
  def main(args: Array[String]) =
    println(new ArrOps(new Arr(Array(1, 2, 3))).fromIterableWithSameElemType)
}

