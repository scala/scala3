import scala.reflect.ClassTag

class Test {
  private def test[A](result: A)(implicit ct: ClassTag[A]): A = result

  test(1, 2) -> Array() // error

}