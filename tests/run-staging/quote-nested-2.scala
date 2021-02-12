import quoted.*
import scala.quoted.staging.*

object Test {
  given Compiler = Compiler.make(getClass.getClassLoader)

  def main(args: Array[String]): Unit = withQuotes {
    val q = '{(q: Quotes) ?=>
      val a = '{4}
      '{${a}}
    }

    println(q.show)
  }
}
