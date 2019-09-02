
import scala.compiletime.testing.typeChecks

object Test {

  def f(s: String) = typeChecks(s) // error
}
