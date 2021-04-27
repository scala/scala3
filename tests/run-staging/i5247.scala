import scala.quoted.*
import scala.quoted.staging.*

object Test {
  given Compiler = Compiler.make(getClass.getClassLoader)
  def main(args: Array[String]): Unit = withQuotes {
    println(foo[Object].show)
    println(bar[Object].show)
  }
  def foo[H : Type](using Quotes): Expr[H] = {
    val t = Type.of[H]
    '{ null.asInstanceOf[t.Underlying] }
  }
  def bar[H : Type](using Quotes): Expr[List[H]] = {
    val t = Type.of[List[H]]
    '{ null.asInstanceOf[t.Underlying] }
  }
}
