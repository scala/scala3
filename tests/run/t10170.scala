object Test {
  def main(args: Array[String]) = println(f)

  def f = {
    val a = 100; ({ val a = 0; (c: Int) => c })(a)
  }
}
