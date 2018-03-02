object Test {

  def main(args: Array[String]): Unit = {
    fun { implicit ghost (x: Int) =>
      println("lambda1")
      "abc"
    }

    fun2 { ghost implicit (x: Int) =>
      println("lambda2")
      "abc"
    }
  }

  def fun(f: implicit ghost Int => String): String = {
    f(35)
  }

  def fun2(f: ghost implicit Int => String): String = {
    f(36)
  }
}
