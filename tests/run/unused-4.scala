object Test {

  def main(args: Array[String]): Unit = {
    fun(foo1)(foo2)
    fun2(foo1)(foo2)
  }

  def foo1: Int = {
    println("foo1")
    42
  }

  def foo2: String = {
    println("foo2")
    "abc"
  }

  def fun(a: Int)(unused b: String): Unit = {
    println("fun " + a)
  }

  def fun2(unused a: Int)(b: String): Unit = {
    println("fun2 " + b)
  }
}
