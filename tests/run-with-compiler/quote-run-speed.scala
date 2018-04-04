import scala.quoted._
import dotty.tools.dotc.quoted.Toolbox._

object Test {

  def powerCode(n: Long, x: Expr[Double]): Expr[Double] =
    if (n == 0) '(1.0)
    else if (n % 2 == 0) '{ { val y = ~x * ~x; ~powerCode(n / 2, '(y)) } }
    else '{ ~x * ~powerCode(n - 1, x) }

  def main(args: Array[String]): Unit = {
    val p = powerCode(1, '(5.0))

    timed("total") {
      for (_ <- 1 to 10)
        timed("run"){
          p.run
        }
    }
  }

  def timed[T](tag: String)(x: => T): T = {
    val t0 = System.nanoTime()
    val res = x
    val t = (System.nanoTime() - t0).toDouble / 1000000
    println(s"$tag: $t")
    res
  }
}
