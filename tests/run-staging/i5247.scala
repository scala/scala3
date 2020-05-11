import scala.quoted._
import scala.quoted.staging._

object Test {
  given Toolbox = Toolbox.make(getClass.getClassLoader)
  def main(args: Array[String]): Unit = usingNewScope {
    println(foo[Object].show)
    println(bar[Object].show)
  }
  def foo[H](using s: Scope)(using s.Type[H]): s.Expr[H] = {
    val t = '[H]
    '{ null.asInstanceOf[$t] }
  }
  def bar[H](using s: Scope)(using s.Type[H]): s.Expr[List[H]] = {
    val t = '[List[H]]
    '{ null.asInstanceOf[$t] }
  }
}
