import scala.quoted._
import scala.quoted.staging._

def foo(n: Int, t: Expr[Int])(using QuoteContext): Expr[Int] =
  if (n == 0) t
  else '{ val a = ${Lifted(n)}; ${foo(n - 1, 'a)} + $t  }

@main def Test = {
  // make available the necessary toolbox for runtime code generation
  given Toolbox = Toolbox.make(getClass.getClassLoader)

  val f: Int = run { foo(2, Lifted(5)) }

  println(f)
}
