object Test {

  def main(args: Array[String]): Unit = {
    var a = 1 + 2
    a = a + 3
    a = 4 + 5 // [break] [step: while]

    while (a * 8 < 100) { // [step: a += 1]
      a += 1              // [step: while] [cont: print]
    }

    println(a) // [break] [cont]
  }
}
