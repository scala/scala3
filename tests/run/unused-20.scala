object Test {

  def main(args: Array[String]): Unit = {
    fun { unused (x: Int) =>
      println("lambda")
      "abc"
    }

  }

  def fun(f: unused Int => String): String = {
    f(35)
  }
}
