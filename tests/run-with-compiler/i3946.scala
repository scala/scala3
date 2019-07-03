import scala.quoted._
object Test {
  def main(args: Array[String]): Unit = {
    implicit val toolbox: scala.quoted.Toolbox = scala.quoted.Toolbox.make(getClass.getClassLoader)
    def u given QuoteContext: Expr[Unit] = '{}
    println(withQuoteContext(u.show))
    println(run(u))
  }
}
