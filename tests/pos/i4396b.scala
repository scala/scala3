import scala.quoted._

class Test {
  def f: Staged[Unit] = '{ case class Foo() }
}
