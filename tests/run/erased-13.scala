object Test {

  def main(args: Array[String]): Unit = {
    erased val x = {
      println("x")
      42
    }
    foo(x)
  }

  def foo erased (a: Int) = {
    println("foo")
  }

}
