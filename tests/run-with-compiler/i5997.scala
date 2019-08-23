import scala.quoted._
import scala.quoted.staging._

object Test {
  delegate for Toolbox = Toolbox.make(getClass.getClassLoader)
  def main(args: Array[String]): Unit = withQuoteContext {
    val v = '{ (if true then Some(1) else None).map(v => v+1) }
    println(v.show)
  }
}
