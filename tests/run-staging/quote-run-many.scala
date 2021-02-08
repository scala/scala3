import scala.quoted.*
import scala.quoted.staging.*

object Test {
  def main(args: Array[String]): Unit = {
    given Compiler = Compiler.make(getClass.getClassLoader)
    def expr(i: Int)(using Quotes) = '{
      val a = 3 + ${Expr(i)}
      2 + a
    }
    for (i <- 0 to 50)
      run(expr(i))
  }
}
