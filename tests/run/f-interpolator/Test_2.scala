
import FInterpolation._

object Test {
  def main(args: Array[String]): Unit = {
    println(ff"integer: ${5}%d")
    println(ff"string: ${"l"}%s")
    println(ff"${5}%s, ${6}%d, ${"hello"}%s")

    val x = 5
    println(ff"$x%d")
  }
}