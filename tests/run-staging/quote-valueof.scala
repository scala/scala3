import scala.quoted._
import scala.quoted.staging._

object Test {

  delegate for Toolbox = Toolbox.make(getClass.getClassLoader)

  def main(args: Array[String]): Unit = withQuoteContext {
    println(('{}).getValue)
    println(('{null}).getValue)
    println(('{true}).getValue)
    println(('{1}).getValue)
    println(('{2: Byte}).getValue)
    println(('{3: Short}).getValue)
    println(('{4}).getValue)
    println(('{5L}).getValue)
    println(('{true}).getValue)
    println(('{3.56f}).getValue)
    println(('{34.5d}).getValue)
    println(('{ 'a' }).getValue)
    println(('{"abc"}).getValue)
  }
}
