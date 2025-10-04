
import scala.deriving.Mirror

class Test[A]
object Test:
  def derived[A](using m: Mirror.Of[A], t: Test[Int]): Test[A] = new Test[A]

case class Foo(i: Int) derives Test
object Foo:
  given i: Test[Int] = new Test[Int]

case class Bar(i: Int) derives Test
object Bar:
  given Test[Int]()
