import dotty.unused

object Test {

  def main(args: Array[String]): Unit = {
    fun(foo)
  }

  def foo = {
    println("foo")
    42
  }
  def fun(@unused boo: Int): Unit = {
    println("fun")
  }
}
