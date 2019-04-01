import scala.quoted._
import scala.quoted.autolift._

object Macros {


  inline def assert(expr: => Boolean): Unit =
    ${ assertImpl('expr) }

  def assertImpl(expr: Expr[Boolean]) =
    '{ if !($expr) then throw new AssertionError(s"failed assertion: ${${showExpr(expr)}}") }


  def showExpr[T](expr: Expr[T]): Expr[String] = expr.toString

  inline def power(inline n: Int, x: Double) = ${ powerCode(n, 'x) }

  def powerCode(n: Int, x: Expr[Double]): Expr[Double] =
    if (n == 0) '{1.0}
    else if (n == 1) x
    else if (n % 2 == 0) '{ { val y = $x * $x; ${ powerCode(n / 2, 'y) } } }
    else '{ $x * ${ powerCode(n - 1, x) } }
}

class Test {

  val program = '{
    import Macros._

    val x = 1
    assert(x != 0)

    ${ assertImpl('{x != 0}) }

    val y = math.sqrt(2.0)

    power(3, y)

    ${ powerCode(3, '{math.sqrt(2.0)}) }
  }

  implicit val toolbox: scala.quoted.Toolbox = scala.quoted.Toolbox.make(getClass.getClassLoader)
  program.run
}
