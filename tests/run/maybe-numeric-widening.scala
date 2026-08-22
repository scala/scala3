//> using options -Yexplicit-nulls
import language.experimental.magic
import scala.magic.*

object WidenBug:
  def f(x: Int): Long? = x        // no numeric widening inserted

object Test:
  def main(args: Array[String]): Unit =
    WidenBug.f(3) match
      case Ok(v) => println("got: " + (v: Long))
      case null  => println("null")
