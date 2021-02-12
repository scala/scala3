
object Test {
  import Macro.*

  def main(args: Array[String]): Unit = {
    println(ff"Hello World ${1}!")
  }
}