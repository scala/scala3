import scala.reflect.ClassTag
object Test {
  def foo[T: ClassTag](x: Any) =
    x match {
      case t: T => true
      case _   => false
    }
  // This is what `foo` expands to
  def foo2[T](x: Any)(implicit ev: ClassTag[T]) =
    x match {
      case t @ ev(_) => true
      case _   => false
    }
  def main(args: Array[String]): Unit = {
    assert(foo[String]("a"))
    assert(!foo[String](new Integer(1)))
    assert(foo[Int](1))
    assert(!foo[Int](true))

    assert(foo2[String]("a"))
    assert(!foo2[String](new Integer(1)))
    assert(foo2[Int](1))
    assert(!foo2[Int](true))
  }
}
