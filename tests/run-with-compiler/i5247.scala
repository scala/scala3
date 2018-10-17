import scala.quoted._
object Test {
  def main(args: Array[String]): Unit = {
    val tb = Toolbox.make(getClass.getClassLoader)
    println(tb.show(foo[Object]))
    println(tb.show(bar[Object]))
  }
  def foo[H : Type]: Staged[H] = {
    val t = '[H]
    '{ null.asInstanceOf[$t] }
  }
  def bar[H : Type]: Staged[List[H]] = {
    val t = '[List[H]]
    '{ null.asInstanceOf[$t] }
  }
}
