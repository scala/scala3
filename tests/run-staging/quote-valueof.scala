import scala.quoted._
import scala.quoted.staging._

object Test {

  given Compiler = Compiler.make(getClass.getClassLoader)

  def main(args: Array[String]): Unit = withQuotes {
    println(('{true}).value)
    println(('{1}).value)
    println(('{2: Byte}).value)
    println(('{3: Short}).value)
    println(('{4}).value)
    println(('{5L}).value)
    println(('{true}).value)
    println(('{3.56f}).value)
    println(('{34.5d}).value)
    println(('{ 'a' }).value)
    println(('{"abc"}).value)
  }
}
