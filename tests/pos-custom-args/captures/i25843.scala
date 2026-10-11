import language.experimental.captureChecking
import scala.deriving.Mirror

trait Show[T]:
  def show(t: T): String

object Show:
  given Show[Int] = _.toString
  given Show[String] = (s: String) => s

  inline def derived[T <: Product](using m: Mirror.ProductOf[T]): Show[T] = t =>
    val showables: List[Show[Any]] = ???
    val values = Tuple.fromProductTyped(t).toList
    showables.zip(values)
    ???

case class Foo(x: Int, y: String) derives Show
