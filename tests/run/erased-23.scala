object Test {

  def main(args: Array[String]): Unit = {
    fun { (given erased x: Int) =>
      println("lambda1")
      "abc"
    }

    fun2 { (given erased x: Int) =>
      println("lambda2")
      "abc"
    }
  }

  def fun(f: (given erased Int) => String): String = {
    f(given 35)
  }

  def fun2(f: (given erased Int) => String): String = {
    f(given 35)
  }
}
