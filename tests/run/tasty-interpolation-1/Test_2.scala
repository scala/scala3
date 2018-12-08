import Macro._

object Test {
  def main(args: Array[String]): Unit = {
    val w = "world"
    println(s2"Hello $w!")
    println(raw2"Hello $w!\n")
    println(foo"Hello $w!")
  }
}
