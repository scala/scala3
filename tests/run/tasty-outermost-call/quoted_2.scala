
import Macros._

object Test {
  def main(args: Array[String]): Unit = {
    println(show(foo0))
    println(show(foo1(4)))
    println(show(foo1(foo0)))
    println(show(foo2(5)))
    println(show(foo2(foo0)))
    println(show(foo2(foo1(foo0))))
    foo1(foo0)
    foo2(foo0)
    foo2(foo1(foo0))
  }

  inline def foo0: Int = 54
  inline def foo1(i: Int): Int = i + i
  inline def foo2(i: Int): Int = foo1(i)

}
