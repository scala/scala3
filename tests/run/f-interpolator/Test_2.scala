
import FInterpolation._

object Test {
  def main(args: Array[String]): Unit = {
    println(ff"abc${5}")
    println(ff"abc${5}asf${"l"}")
    println(ff"abc${5}${6}asf${8}")
  }
}