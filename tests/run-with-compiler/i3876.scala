import dotty.tools.dotc.quoted.Runners._
import scala.quoted._
object Test {
  def main(args: Array[String]): Unit = {
    val x: Expr[Int] = '(3)

    val f: Expr[Int => Int] = '{ (x: Int) => x + x }
    println(f(x).run)
    println(f(x).show)

    val f2: Expr[Int => Int] = '{
      def f(x: Int): Int = x + x
      f
    }
    println(f2(x).run)
    println(f2(x).show)

    val f3: Expr[Int => Int] = '{
      val f: (x: Int) => Int = x => x + x
      f
    }
    println(f3(x).run)
    println(f3(x).show) // TODO improve printer

    val f4: Expr[Int => Int] = '{
      inlineLambda
    }
    println(f4(x).run)
    println(f4(x).show)
  }

  inline def inlineLambda: Int => Int = x => x + x
}