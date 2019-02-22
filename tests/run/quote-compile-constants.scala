import scala.quoted._

object Test {
  def main(args: Array[String]): Unit = {
    println(('{true}).getClass)
    println(('{ 'a' }).getClass)
    println(('{ '\n' }).getClass)
    println(('{ '"' }).getClass)
    println(('{ '\'' }).getClass)
    println(('{ '\\' }).getClass)
    println(('{1}).getClass)
    println(('{ { { 2 } } }).getClass)
    println(('{3L}).getClass)
    println(('{4.0f}).getClass)
    println(('{5.0d}).getClass)
    println(('{"xyz"}).getClass)
    println(('{}).getClass)
    println(('{()}).getClass)
    println(('{{()}}).getClass)
  }
}
