import scala.quoted._
import scala.quoted.staging._

object Test {

  given Toolbox = Toolbox.make(getClass.getClassLoader)

  def main(args: Array[String]): Unit = withQuotes {
    println(('{}).unlift)
    println(('{true}).unlift)
    println(('{1}).unlift)
    println(('{2: Byte}).unlift)
    println(('{3: Short}).unlift)
    println(('{4}).unlift)
    println(('{5L}).unlift)
    println(('{true}).unlift)
    println(('{3.56f}).unlift)
    println(('{34.5d}).unlift)
    println(('{ 'a' }).unlift)
    println(('{"abc"}).unlift)
  }
}
