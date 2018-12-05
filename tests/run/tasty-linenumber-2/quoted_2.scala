
import LineNumber._

object Test {
  def main(args: Array[String]): Unit = {
    foo(line)
    foo

    foo
    foo
  }

  def foo(implicit line: LineNumber): Unit = {
    println("foo " + line)
  }
}
