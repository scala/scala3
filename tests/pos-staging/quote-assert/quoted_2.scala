
import scala.quoted.*
import scala.quoted.staging.*

import Macros.*

object Test {

  inline def assert(expr: => Boolean): Unit =
    ${ assertImpl('expr) }


  def program(using Quotes) = '{
    val x = 1
    assert(x != 0)

    ${ assertImpl('{x != 0}) }
  }

  given Compiler = Compiler.make(getClass.getClassLoader)
  run(program)
}
