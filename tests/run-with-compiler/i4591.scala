import scala.quoted.Toolbox.Default._
import scala.quoted._

object Test {

  def foo[T: Type](init: Expr[T]): Expr[Unit] = '{
    var x = $init
    println(x)
  }

  def main(args: Array[String]): Unit = {
    foo('{Option(9)}).run
  }

}
