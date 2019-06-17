import scala.quoted._

object Test {

  def foo[T: Type](init: Expr[T]): Expr[Unit] = '{
    var x = $init
    println(x)
  }

  def main(args: Array[String]): Unit = {
    implicit val toolbox: scala.quoted.Toolbox = scala.quoted.Toolbox.make(getClass.getClassLoader)
    run(foo('{Option(9)}))
  }

}
