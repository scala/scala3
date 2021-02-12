import scala.quoted.*
import scala.quoted.staging.*

object Macros {


  inline def assert(expr: => Boolean): Unit =
    ${ assertImpl('expr) }

  def assertImpl(expr: Expr[Boolean])(using Quotes) =
    '{ if !($expr) then throw new AssertionError(s"failed assertion: ${${showExpr(expr)}}") }


  def showExpr[T](expr: Expr[T])(using Quotes): Expr[String] = Expr(expr.toString)

  inline def power(inline n: Int, x: Double) = ${ powerCode('n, 'x) }

  def powerCode(n: Expr[Int], x: Expr[Double]) (using Quotes): Expr[Double] =
    powerCode(n.valueOrError, x)

  def powerCode(n: Int, x: Expr[Double])(using Quotes): Expr[Double] =
    if (n == 0) '{1.0}
    else if (n == 1) x
    else if (n % 2 == 0) '{ { val y = $x * $x; ${ powerCode(n / 2, 'y) } } }
    else '{ $x * ${ powerCode(n - 1, x) } }
}

class Test {

  given Compiler = Compiler.make(getClass.getClassLoader)

  run {
    val program = '{
      import Macros.*

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
