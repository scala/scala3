object Test {
  import Macro.transformAndPrint

  class Box(val x: Int) {
    def >(y: Int): Boolean = x > y
    def >(b: Box): Boolean = x > b.x
  }

  def main(args: Array[String]): Unit = {
    val a: Int = 100
    transformAndPrint(a > 5)
    transformAndPrint(a > 5.0)
    transformAndPrint(a > 'a')

    val b1 = new Box(10)
    val b2 = new Box(3)
    transformAndPrint(b1 > 4)
    transformAndPrint(b1 > b2)

    val b: Boolean = false
    transformAndPrint(!b)

    transformAndPrint(true)
    transformAndPrint(false)
    transformAndPrint(if (!b) a > 5 else b1 > 4)

    var b3: Boolean = false
    transformAndPrint(while (b3) { println(b3); b3 = false })

    println("end")
  }
}
