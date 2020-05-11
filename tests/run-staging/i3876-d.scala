import scala.quoted._
import scala.quoted.staging._
object Test {
  def main(args: Array[String]): Unit = {
    given Toolbox = Toolbox.make(getClass.getClassLoader)

    def x(using s: Scope): s.Expr[Int] = '{3}

    def f4(using s: Scope): s.Expr[Int => Int] = '{
      inlineLambda
    }
    println(run(Expr.betaReduce('{$f4($x)})))
    println(usingNewScope(Expr.betaReduce('{$f4($x)}).show))
  }

  transparent inline def inlineLambda: Int => Int = x => x + x
}