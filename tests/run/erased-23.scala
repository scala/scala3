object Test {

  def main(args: Array[String]): Unit = {
    fun { erased (x: Int) |=>
      println("lambda1")
      "abc"
    }
  }

  def fun(f: erased Int |=> String): String = {
    f with 35
  }
}
