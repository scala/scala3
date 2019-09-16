import scala.quoted._
import scala.quoted.staging._
import scala.quoted.autolift.given

object Macros {


  inline def assert(expr: => Boolean): Unit =
    ${ assertImpl('expr) }

  def assertImpl(expr: Expr[Boolean])(given QuoateContext) =
    '{ if !($expr) then throw new AssertionError(s"failed assertion: ${${showExpr(expr)}}") }


  def showExpr[T](expr: Expr[T])(given QuoateContext): Expr[String] = expr.toString

  inline def power(inline n: Int, x: Double) = ${ powerCode(n, 'x) }

  def powerCode(n: Int, x: Expr[Double])(given QuoateContext): Expr[Double] =
    if (n == 0) '{1.0}
    else if (n == 1) x
    else if (n % 2 == 0) '{ { val y = $x * $x; ${ powerCode(n / 2, 'y) } } }
    else '{ $x * ${ powerCode(n - 1, x) } }
}

class Test {

  given as Toolbox = Toolbox.make(getClass.getClassLoader)

  run {
    val program = '{
      import Macros._

      val x = 1
      assert(x != 0)

      ${ assertImpl('{x != 0}) }

      val y = math.sqrt(2.0)

      power(3, y)

      ${ powerCode(3, '{math.sqrt(2.0)}) }
    }

    program
  }
}
