// Uses structural types; therefore not expressible in dotty
import scala.annotation.StaticAnnotation

class ann(val bar: Any) extends StaticAnnotation

object Api {
  @ann({def foo = "foo!!"})
  def foo = println("foo")
}
