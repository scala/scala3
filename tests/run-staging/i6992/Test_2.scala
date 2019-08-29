import macros._

object Test {
  val foo = new Foo

  def main(args: Array[String]) = {
    println(mcr {foo})
  }
}
