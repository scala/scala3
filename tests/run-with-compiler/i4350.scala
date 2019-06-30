
import scala.quoted._

class Foo[T: Type] {
  def q given QuoteContext = '{(null: Any).asInstanceOf[T]}
}

object Test {
  implicit val toolbox: scala.quoted.Toolbox = scala.quoted.Toolbox.make(getClass.getClassLoader)
  def main(args: Array[String]): Unit = withQuoteContext {
    println((new Foo[Object]).q.show)
    println((new Foo[String]).q.show)
  }
}
