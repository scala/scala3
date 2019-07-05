import scala.quoted._

object Test {
  implicit val toolbox: scala.quoted.Toolbox = scala.quoted.Toolbox.make(getClass.getClassLoader)

  def main(args: Array[String]): Unit = {
    def res given QuoteContext = '{
      val x: Option[Int] = Option(3)
      if (x.isInstanceOf[Some[_]]) Option(1)
      else None
    }
    println("show0 : " + withQuoteContext(res.show))
    println("run1 : " + run(res))
    println("run2 : " + run(res))
    println("show3 : " + withQuoteContext(res.show))
  }
}
