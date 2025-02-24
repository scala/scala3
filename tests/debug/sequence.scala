object Test {
  def main(args: Array[String]): Unit = {
    var a = 1 + 2     // [break] [step: a + 3]
    a = a + 3         // [step: 4 + 5]
    a = 4 + 5         // [step: a * 8]
    a = a * 8         // [step: 9 * 9]
    a = 9 * 9         // [step: 34 * 23]
    a = 34 * 23       // [step: print]
    println(a)          // [cont]
  }
}