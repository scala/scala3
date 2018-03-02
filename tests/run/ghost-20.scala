object Test {

  def main(args: Array[String]): Unit = {
    fun { ghost (x: Int) =>
      println("lambda")
      "abc"
    }

  }

  def fun(f: ghost Int => String): String = {
    f(35)
  }
}
