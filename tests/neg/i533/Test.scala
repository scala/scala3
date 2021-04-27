object Test {
  def main(args: Array[String]): Unit = {
    val x = new Array[Int](1)
    x(0) = 10
    println(JA.get(x)) // error
    println(JA.getVarargs(x*)) // now OK.
  }
}
