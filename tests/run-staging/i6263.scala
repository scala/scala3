import quoted.*
import scala.quoted.staging.*

object Test {

  given Compiler = Compiler.make(getClass.getClassLoader)

  def main(args: Array[String]): Unit = withQuotes {
    fn("foo")
    fn((1,2))
    fn(O)
    fn(1)
  }

  def fn[T : Type](v : T) = "ok"
}

object O
