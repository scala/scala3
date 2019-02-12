
import Macros._

object Test {
  def main(args: Array[String]): Unit = {
    println(withSource(foo("abc")))
    println(withSource( foo(  "def"  )))
    println(withSource(foo("ghi" /* comment */)))
  }

  def foo(x: Any): Any = x

}
