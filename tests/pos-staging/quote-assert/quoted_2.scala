
import scala.quoted._
import scala.quoted.staging._

import Macros._

object Test {

  inline def assert(expr: => Boolean): Unit =
    ${ assertImpl('expr) }


  def program(given QuoteContext) = '{
    val x = 1
    assert(x != 0)

    ${ assertImpl('{x != 0}) }
  }

  given Toolbox = Toolbox.make(getClass.getClassLoader)
  run(program)
}
