//> using options -Werror -Wunused:all
import scala.util.NotGiven

object Test {
  def f[T](a: Int)(using NotGiven[T <:< Int]) = a + 2
}

trait Furthermore:
  type Intish[A] = A <:< Int
  def f[A: Intish] = ()
