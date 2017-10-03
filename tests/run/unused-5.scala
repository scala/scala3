object Test {

  def main(args: Array[String]): Unit = {
    fun(foo(1))(foo(2))(foo(3))(foo(4))
    fun2(foo(1))(foo(2))(foo(3))(foo(4))
  }

  def foo(i: Int): Int = {
    println("foo")
    i
  }

  def fun(a: Int)(unused b: Int)(c: Int)(unused d: Int): Unit = {
    println("fun " + a + " " + c)
  }

  def fun2(unused a2: Int)(b2: Int)(unused c2: Int)(d2: Int): Unit = {
    println("fun2 " + b2 + " " + d2)
  }
}
