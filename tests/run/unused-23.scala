object Test {

  def main(args: Array[String]): Unit = {
    fun { implicit unused (x: Int) =>
      println("lambda1")
      "abc"
    }

    fun2 { unused implicit (x: Int) =>
      println("lambda2")
      "abc"
    }
  }

  def fun(f: implicit unused Int => String): String = {
    f(35)
  }

  def fun2(f: unused implicit Int => String): String = {
    f(36)
  }
}
