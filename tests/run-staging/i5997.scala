import scala.quoted.*
import scala.quoted.staging.*

object Test {
  given Compiler = Compiler.make(getClass.getClassLoader)
  def main(args: Array[String]): Unit = withQuotes {
    val v = '{ (if true then Some(1) else None).map(v => v+1) }
    println(v.show)
  }
}
