import quoted._
import scala.quoted.staging._

object Test {
  given Toolbox = Toolbox.make(getClass.getClassLoader)
  def main(args: Array[String]): Unit = withQuotes {
    val q = '{ (using q: Quotes) => '{3} }
    println(q.show)
  }
}
