
import scala.quoted._
import scala.quoted.staging._

class Foo[T: Type] {
  def q(using Quotes) = '{(null: Any).asInstanceOf[T]}
}

object Test {
  given Compiler = Compiler.make(getClass.getClassLoader)
  def main(args: Array[String]): Unit = withQuotes {
    println((new Foo[Object]).q.show)
    println((new Foo[String]).q.show)
  }
}
