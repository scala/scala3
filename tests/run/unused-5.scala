
import dotty.unused

object Test {

  def main(args: Array[String]): Unit = {
    fun(foo)(foo)(foo)(foo)
    fun2(foo)(foo)(foo)(foo)
  }

  def foo: Int = {
    println("foo")
    42
  }

  def fun(a: Int)(@unused b: Int)(c: Int)(@unused d: Int): Unit = {
    println("fun")
  }

  def fun2(@unused a2: Int)(b2: Int)(@unused c2: Int)(d2: Int): Unit = {
    println("fun2")
  }
}

