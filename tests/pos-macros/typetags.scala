import scala.quoted._

object Test {

  def f[T: Staged](using QuoteContext) = {
    implicitly[Staged[Int]]
    implicitly[Staged[List[Int]]]
    implicitly[Staged[T]]
    implicitly[Staged[List[T]]]
  }
}
