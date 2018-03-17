object Test {

  def main(args: Array[String]): Unit = {
    fun { erased (x: Int) =>
      println("lambda")
      "abc"
    }

  }

  def fun(f: erased Int => String): String = {
    f(35)
  }
}
