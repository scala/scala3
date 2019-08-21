
import scala.quoted._
import scala.quoted.staging._

class Foo[T: Type] {
  def q given QuoteContext = '{(null: Any).asInstanceOf[T]}
}

object Test {
  implicit val toolbox: scala.quoted.staging.Toolbox = scala.quoted.staging.Toolbox.make(getClass.getClassLoader)
  def main(args: Array[String]): Unit = withQuoteContext {
    println((new Foo[Object]).q.show)
    println((new Foo[String]).q.show)
  }
}
