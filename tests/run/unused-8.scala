import dotty.unused

object Test {

  def main(args: Array[String]): Unit = {
    fun(foo)
  }

  def foo = {
    println("foo")
    42
  }

  @unused def fun(@unused a: Int): Int = {
    println("fun")
    a
  }
}
