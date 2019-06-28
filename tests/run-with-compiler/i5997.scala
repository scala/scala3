import scala.quoted._
object Test {
  implicit val toolbox: scala.quoted.Toolbox = scala.quoted.Toolbox.make(getClass.getClassLoader)
  def main(args: Array[String]): Unit = withQuoteContext {
    val v = '{ (if true then Some(1) else None).map(v => v+1) }
    println(v.show)
  }
}
