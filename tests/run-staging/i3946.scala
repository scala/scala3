import scala.quoted._
import scala.quoted.staging._
object Test {
  def main(args: Array[String]): Unit = {
    given Compiler = Compiler.make(getClass.getClassLoader)
    def u(using Quotes): Expr[Unit] = '{}
    println(withQuotes(u.show))
    println(run(u))
  }
}
