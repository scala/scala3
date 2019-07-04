import scala.quoted._

object Test {

  inline def foo(x: Expr[Int]) given QuoteContext: Expr[Int] = '{
    println("foo")
    $x
  }

  implicit val toolbox: scala.quoted.Toolbox = scala.quoted.Toolbox.make(getClass.getClassLoader)

  def main(args: Array[String]): Unit = withQuoteContext {
    val y = '{45}
    println(foo(y).show)
  }

}
