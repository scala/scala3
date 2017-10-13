object Test {

  def main(args: Array[String]): Unit = {
    fun { implicit unused (x: Int) =>
      println("lambda")
      "abc"
    }

  }

  def fun(f: implicit unused Int => String): String = {
    f(35)
  }
}
