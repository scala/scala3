import scala.quoted._
import Macros._

class Test {

  inline def assert(expr: => Boolean): Unit =
    ~ assertImpl('(expr))


  val program = '{
    val x = 1
    assert(x != 0)

    ~assertImpl('(x != 0))
  }

  program.run
}
