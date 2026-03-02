object Test {

  def main(args: Array[String]): Unit = {
    var a = 1 + 2
    a = a + 3
    a = 4 + 5

    if (a * 8 > 20)
      a = 9 * 9
    else
      a = 34 * 23

    if (a * 8 < 20)
      a = 9 * 9
    else
      a = 34 * 23

    println(a)
  }
}
