object Test {

  def main(args: Array[String]): Unit = {
    fun(foo)(foo)
  }

  def foo: Int = {
    println("foo")
    42
  }

  def fun(unused a: Int)(unused b: Int): Unit = {
    println("fun")
  }

}
