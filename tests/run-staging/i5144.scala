import scala.quoted.*
import scala.quoted.staging.*

object Test {
  given Compiler = Compiler.make(getClass.getClassLoader)
  def eval1(ff: Expr[Int => Int])(using Quotes): Expr[Int] = '{$ff(42)}

  def peval1()(using Quotes): Expr[Unit] = '{
    def f(x: Int): Int = ${eval1('f)}
  }

  def main(args: Array[String]): Unit = withQuotes {
    val p = peval1()
    println(p.show)
  }

}