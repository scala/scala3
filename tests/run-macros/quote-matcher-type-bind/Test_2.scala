import Macros.*


object Test {

  def main(args: Array[String]): Unit = {
    swapFandG(DSL.f(5))
    swapFandG(DSL.g("abc"))
  }

}
