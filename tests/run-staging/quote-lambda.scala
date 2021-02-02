import scala.quoted._
import scala.quoted.staging._

object Test {
  given Compiler = Compiler.make(getClass.getClassLoader)
  def main(args: Array[String]): Unit = withQuotes {
    '{ (x: Int) => ${'x} }
  }
}
