import scala.quoted._
import scala.quoted.staging._

object Test {
  given as Toolbox = Toolbox.make(getClass.getClassLoader)
  def main(args: Array[String]): Unit = withQuoteContext {
    '{ (x: Int) => ${'x} }
  }
}
