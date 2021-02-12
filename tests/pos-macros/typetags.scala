import scala.quoted.*

object Test {

  def f[T: Type](using Quotes) = {
    implicitly[Type[Int]]
    implicitly[Type[List[Int]]]
    implicitly[Type[T]]
    implicitly[Type[List[T]]]
  }
}
