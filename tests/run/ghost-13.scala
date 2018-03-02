object Test {

  def main(args: Array[String]): Unit = {
    ghost val x = {
      println("x")
      42
    }
    foo(x)
  }

  def foo(ghost a: Int) = {
    println("foo")
  }

}
