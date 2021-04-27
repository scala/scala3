import scala.quoted.*
import scala.quoted.staging.*

object Test {
  given Compiler = Compiler.make(getClass.getClassLoader)

  def main(args: Array[String]): Unit = withQuotes {
    var e = '{1}
    e = '{$e + 1}
    e = '{$e + 2}
    e = '{$e + 3}
    println(e.show)
  }
}
