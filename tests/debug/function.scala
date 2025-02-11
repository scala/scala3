object Test {
  def main(args: Array[String]): Unit = {
    val a = 1 + 2
    val b = a * 9        // [break] [step: plus] [step: c = plus]
    val plus = (x: Int, y: Int) => {    // [cont: x * x]
      val a = x * x                     // [break] [step: y * y]
      val b = y * y                     // [step: a + b]
      a + b                             // [next] [next]
    }
    val c = plus(a, b)   // [next: print]
    println(c)             // [cont]
  }

}
