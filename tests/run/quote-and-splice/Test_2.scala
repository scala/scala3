object Test {
  import Macros._

  def main(args: Array[String]): Unit = {
    println(macro1)
    println(macro2(true))
    println(macro2(false))
    println(macro3(1))
    println(macro4(2)(3))
    println(macro5(4, 5))
    println(power(0, 5))
    println(power(1, 5))
    println(power(2, 5))
    println(power(3, 5))
  }

}
