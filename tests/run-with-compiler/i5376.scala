import scala.quoted._

object Test {
  implicit val toolbox: scala.quoted.Toolbox = scala.quoted.Toolbox.make(getClass.getClassLoader)

  def main(args: Array[String]): Unit = withQuoteContext {
    var e = '{1}
    e = '{$e + 1}
    e = '{$e + 2}
    e = '{$e + 3}
    println(e.show)
  }
}
