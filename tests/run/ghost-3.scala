object Test {

  def main(args: Array[String]): Unit = {
    fun(foo1)(foo2)
  }

  def foo1: Int = {
    println("foo1")
    42
  }

  def foo2: String = {
    println("foo2")
    "abc"
  }

  def fun(ghost a: Int)(ghost b: String): Unit = {
    println("fun")
  }

}
