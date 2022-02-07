import scala.deriving._

object Test:
  def main(args: Array[String]): Unit =
    println(summon[Mirror.Of[TreeValue]]) // error
