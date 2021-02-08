import scala.quoted.*
import scala.quoted.staging.*

object Test {

  given Compiler = Compiler.make(getClass.getClassLoader)
  def main(args: Array[String]): Unit = {
    withQuotes(Type.of[List])

    def list(using Quotes) = bound('{List(1, 2, 3)})
    println(withQuotes(list.show))
    println(run(list))

    def opt(using Quotes) = bound('{Option(4)})
    println(withQuotes(opt.show))
    println(run(opt))

    def map(using Quotes) = bound('{Map(4 -> 1)})
    println(withQuotes(map.show))
    println(run(map))
  }

  def bound[T: Type, S[_]: Type](x: Expr[S[T]])(using Quotes): Expr[S[T]] = '{
    val y: S[T] = $x
    y
  }
}
