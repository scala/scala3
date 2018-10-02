
object Test {
  import Macro._

  def main(args: Array[String]): Unit = {
    println(ff"Hello World ${1}!")
  }
}