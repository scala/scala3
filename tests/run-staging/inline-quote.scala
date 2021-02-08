import scala.quoted.*
import scala.quoted.staging.*

object Test {

  inline def foo(x: Expr[Int])(using Quotes): Expr[Int] = '{
    println("foo")
    $x
  }

  given Compiler = Compiler.make(getClass.getClassLoader)

  def main(args: Array[String]): Unit = withQuotes {
    val y = '{45}
    println(foo(y).show)
  }

}
