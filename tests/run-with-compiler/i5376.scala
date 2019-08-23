import scala.quoted._
import scala.quoted.staging._

object Test {
  delegate for Toolbox = Toolbox.make(getClass.getClassLoader)

  def main(args: Array[String]): Unit = withQuoteContext {
    var e = '{1}
    e = '{$e + 1}
    e = '{$e + 2}
    e = '{$e + 3}
    println(e.show)
  }
}
