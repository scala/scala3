import quoted.*
import scala.quoted.staging.*

object Test {
  given Compiler = Compiler.make(getClass.getClassLoader)

  def main(args: Array[String]): Unit = withQuotes {
    val q = '{
      type T[X] = List[X]
      val x = "foo"
      ${
        val y = 'x
        '{ val z: T[String] = List($y) }
      }
      x
    }

    println(q.show)
  }
}
