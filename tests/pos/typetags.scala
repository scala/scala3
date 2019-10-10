import scala.quoted._

object Test {

  def f[T: TypeTag](given QuoteContext) = {
    implicitly[TypeTag[Int]]
    implicitly[TypeTag[List[Int]]]
    implicitly[TypeTag[T]]
    implicitly[TypeTag[List[T]]]
  }
}
