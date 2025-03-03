object Test {

  def main(args: Array[String]): Unit = {
    var a = 1 + 2 // [break] [step: a + 3]
    a = a + 3     // [step: 4 + 5]
    a = 4 + 5     // [step: if]

    if (a * 8 > 20)  // [step: 9 * 9]
      a = 9 * 9      // [step: if]
    else
      a = 34 * 23

    if (a * 8 < 20)  // [step: 34 * 23]
      a = 9 * 9
    else
      a = 34 * 23    // [step: print]

    println(a)
  }
}
