import scala.quoted.*
import scala.quoted.staging.*

def foo(n: Int, t: Expr[Int])(using Quotes): Expr[Int] =
  if (n == 0) t
  else '{ val a = ${Expr(n)}; ${foo(n - 1, 'a)} + $t  }

object Test:
  def main(args: Array[String]) = {
    // make available the necessary toolbox for runtime code generation
    given Compiler = Compiler.make(getClass.getClassLoader)

    val f: Int = run { foo(2, Expr(5)) }

    println(f)
  }
