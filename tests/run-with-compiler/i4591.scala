import scala.quoted._

object Test {

  def foo[T: Type](init: Expr[T]): Staged[Unit] = '{
    var x = $init
    println(x)
  }

  def main(args: Array[String]): Unit = {
    val tb = Toolbox.make
    tb.run(foo('{Option(9)}))
  }

}
