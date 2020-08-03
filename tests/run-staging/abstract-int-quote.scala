import scala.quoted._
import scala.quoted.staging._

object Test:

  given Toolbox = Toolbox.make(getClass.getClassLoader)

  def main(args: Array[String]): Unit =
    def reduce[T: Staged](using QuoteContext)(succ: Expr[T] => Expr[T], zero: Expr[T]): Expr[T] = '{
      var z = $zero
      ${ succ('z) }
    }
    def resCode2(using QuoteContext): Expr[Int] =
      reduce[Int](x => '{$x + 1}, '{0})

    println(withQuoteContext(resCode2.show))
