import dotty.tools.dotc.quoted.Toolbox._
import scala.quoted._
import Macros._

object Test {

  inline def assert(expr: => Boolean): Unit =
    ~ assertImpl('(expr))


  val program = '{
    val x = 1
    assert(x != 0)

    ~assertImpl('(x != 0))
  }

  program.run
}
