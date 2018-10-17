import scala.quoted._

object Test {

  def f[T: Type](implicit st: StagingContext) = {
    implicitly[Type[Int]]
    implicitly[Type[List[Int]]]
    implicitly[Type[T]]
    implicitly[Type[List[T]]]
  }
}
