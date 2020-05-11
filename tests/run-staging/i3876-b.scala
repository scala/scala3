import scala.quoted._
import scala.quoted.staging._
object Test {
  def main(args: Array[String]): Unit = {
    given Toolbox = Toolbox.make(getClass.getClassLoader)

    def x(using s: Scope): s.Expr[Int] = '{3}

    def f2(using s: Scope): s.Expr[Int => Int] = '{
      def f(x: Int): Int = x + x
      f
    }

    def expr(using Scope) = '{$f2($x)}
    println(run(Expr.betaReduce(expr)))
    println(usingNewScope(Expr.betaReduce(expr).show)) // TODO improve printer
  }
}
