import scala.quoted._
object Test {
  implicit val toolbox: scala.quoted.Toolbox = scala.quoted.Toolbox.make(getClass.getClassLoader)
  def main(args: Array[String]): Unit = withQuoteContext {
    println(foo[Object].show)
    println(bar[Object].show)
  }
  def foo[H : Type]: Expr[H] = {
    val t = '[H]
    '{ null.asInstanceOf[$t] }
  }
  def bar[H : Type]: Expr[List[H]] = {
    val t = '[List[H]]
    '{ null.asInstanceOf[$t] }
  }
}
