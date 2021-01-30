import quoted.*
import scala.quoted.staging.*

object Test {
  def main(args: Array[String]): Unit = {
    given Compiler = Compiler.make(getClass.getClassLoader)
    def q(using Quotes) = f
    println(run(q))
    println(withQuotes(q.show))
  }

  def f(using Quotes): Expr[Int] = '{
    def ff: Int = {
      $g
    }
    ff
  }

  def g(using Quotes): Expr[Int] = '{
    val a = 9
    a + 0
  }
}
