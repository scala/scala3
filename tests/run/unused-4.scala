import dotty.unused2

object Test {

  def main(args: Array[String]): Unit = {
    fun(foo)(foo2)
    fun2(foo)(foo2)
  }

  def foo: Int = {
    println("foo")
    42
  }

  def foo2: String = {
    println("foo2")
    "abc"
  }

  def fun(a: Int)(unused b: String): Unit = {
    println("fun")
  }

  def fun2(@unused2 a: Int)(b: String): Unit = {
    println("fun2")
  }
}

