import scala.quoted.*
import scala.quoted.staging.*
object Test {
  def main(args: Array[String]): Unit = {
    given Compiler = Compiler.make(getClass.getClassLoader)
    def u(using Quotes): Expr[Unit] = '{}
    println(withQuotes(u.show))
    println(run(u))
  }
}
