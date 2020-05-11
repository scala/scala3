import scala.quoted._
import scala.quoted.staging._

object Macros {


  inline def assert(expr: => Boolean): Unit =
    ${ assertImpl('expr) }

  def assertImpl(using s: Scope)(expr: s.Expr[Boolean]) =
    '{ if !($expr) then throw new AssertionError(s"failed assertion: ${${showExpr(expr)}}") }


  def showExpr[T](using s: Scope)(expr: s.Expr[T]): s.Expr[String] = Expr(expr.toString)

  inline def power(inline n: Int, x: Double) = ${ powerCode0('n, 'x) }

  def powerCode0(using s: Scope)(n: s.Expr[Int], x: s.Expr[Double]): s.Expr[Double] =
    powerCode(n.unliftOrError, x)

  def powerCode(using s: Scope)(n: Int, x: s.Expr[Double]): s.Expr[Double] =
    if (n == 0) '{1.0}
    else if (n == 1) x
    else if (n % 2 == 0) '{ { val y = $x * $x; ${ powerCode(n / 2, 'y) } } }
    else '{ $x * ${ powerCode(n - 1, x) } }
}

class Test {

  given Toolbox = Toolbox.make(getClass.getClassLoader)

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
