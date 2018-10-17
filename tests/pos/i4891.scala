import scala.quoted._

object Test {
  def foo: Staged[Option[String]] = '{None}
}
