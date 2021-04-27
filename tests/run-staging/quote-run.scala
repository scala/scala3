import scala.quoted.*
import scala.quoted.staging.*

object Test {
  def main(args: Array[String]): Unit = {
    given Compiler = Compiler.make(getClass.getClassLoader)
    def expr(using Quotes) = '{
      val a = 3
      println("foo")
      2 + a
    }
    println(run(expr))
    println(run(expr))
    println(withQuotes(expr.show))
  }
}
