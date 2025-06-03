
object Power:
  import scala.math.pow as power
  import scala.quoted.*
  inline def powerMacro(x: Double, inline n: Int): Double = ${ powerCode('x, 'n) }
  def powerCode(x: Expr[Double], n: Expr[Int])(using Quotes): Expr[Double] =
    n match
    case Expr(m) => unrolledPowerCode(x, m)
    case _ => '{ power($x, $n.toDouble) }
  def unrolledPowerCode(x: Expr[Double], n: Int)(using Quotes): Expr[Double] =
    n match
    case 0 => '{ 1.0 }
    case 1 => x
    case _ => '{ $x * ${ unrolledPowerCode(x, n - 1) } }
