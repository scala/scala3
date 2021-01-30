import scala.quoted.*
import scala.quoted.staging.*

object Test {

  def foo[T: Type](init: Expr[T])(using Quotes): Expr[Unit] = '{
    var x = $init
    println(x)
  }

  def main(args: Array[String]): Unit = {
    given Compiler = Compiler.make(getClass.getClassLoader)
    run(foo('{Option(9)}))
  }

}
