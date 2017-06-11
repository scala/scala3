trait Eq[T] {
  def eq(a: T, b: T): Boolean
}

object Eq {
  implicit object int extends Eq[Int] {
    def eq(a: Int, b: Int) = a == b
  }
}

object Test {
  def f[T](a: T, b: T)(implicit T: Eq[T]) = T.eq(a, b)

  def main(args: Array[String]) =
    assert(!f(1, 2))
}
