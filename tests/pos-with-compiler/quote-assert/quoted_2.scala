
import scala.quoted._
import Macros._

object Test {

  inline def assert(expr: => Boolean): Unit =
    ${ assertImpl('expr) }


  val program = '{
    val x = 1
    assert(x != 0)

    ${ assertImpl('{x != 0}) }
  }

  implicit val toolbox: scala.quoted.Toolbox = scala.quoted.Toolbox.make(getClass.getClassLoader)
  program.run
}
