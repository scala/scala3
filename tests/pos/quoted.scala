import scala.quoted._

class Test {

  object Macros {

    inline def assert(expr: => Boolean): Unit =
      ~ assertImpl('(expr))

    def assertImpl(expr: Expr[Boolean]) =
      '{ if !(~expr) then throw new AssertionError(s"failed assertion: ${~expr}") }

  }

  val program = '{
    import Macros._

    val x = 1
    assert(x != 0)

    ~assertImpl('(x != 0))
  }

  program.run
}
