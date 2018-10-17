import scala.quoted._

class Test {
  def f: Staged[Int] =
    '{ Option(4) match { case Some(a) => a; case None => 1 }}
}