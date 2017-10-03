import dotty.unused

object Test {

  def main(args: Array[String]): Unit = {
    @unused val x = {
      println("x")
      42
    }
    foo(x)
  }

  def foo(@unused a: Int) = {
    println("foo")
  }

}
