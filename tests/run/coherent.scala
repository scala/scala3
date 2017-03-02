trait Base[T] extends scala.typeclass.Coherent {
  def value: T
}

class Sub1[T](val value: T) extends Base[T]
class Sub2[T](val value: T) extends Base[T]

object Test {
  implicit class ValueDeco[T: Base](x: T) {
    def value = implicitly[Base[T]].value
  }
  def f[T](t: T)(implicit ev1: Sub1[T], ev2: Sub2[T]) = {
    // the next four lines give errors if `Base` is not declared coherent
    assert(t.value == 2)
    assert(implicitly[Base[T]].value == 2)
    assert(implicitly[Base[T]].value == 2)
    assert(t.value == 2)
  }
  implicit val s1: Sub1[Int] = new Sub1(2)
  implicit val s2: Sub2[Int] = new Sub2(3) // This is not coherent, just used to show that first alternative will be chosen.

  def main(args: Array[String]) = {
    f(1)
  }
}
