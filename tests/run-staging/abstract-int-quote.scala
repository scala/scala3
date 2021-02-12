import scala.quoted.*
import scala.quoted.staging.*

object Test:

  given Compiler = Compiler.make(getClass.getClassLoader)

  def main(args: Array[String]): Unit =
    def reduce[T: Type](using Quotes)(succ: Expr[T] => Expr[T], zero: Expr[T]): Expr[T] = '{
      var z = $zero
      ${ succ('z) }
    }
    def resCode2(using Quotes): Expr[Int] =
      reduce[Int](x => '{$x + 1}, '{0})

    println(withQuotes(resCode2.show))
