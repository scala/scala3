import scala.quoted._

object Test {

  def f[T: Type] with QuoteContext = {
    implicitly[Type[Int]]
    implicitly[Type[List[Int]]]
    implicitly[Type[T]]
    implicitly[Type[List[T]]]
  }
}
