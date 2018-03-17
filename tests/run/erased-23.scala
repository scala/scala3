object Test {

  def main(args: Array[String]): Unit = {
    fun { implicit erased (x: Int) =>
      println("lambda1")
      "abc"
    }

    fun2 { erased implicit (x: Int) =>
      println("lambda2")
      "abc"
    }
  }

  def fun(f: implicit erased Int => String): String = {
    f(35)
  }

  def fun2(f: erased implicit Int => String): String = {
    f(36)
  }
}
