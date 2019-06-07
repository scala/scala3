
import Macros._

object Test {
  def main(args: Array[String]): Unit = {
    val a: String = "abc"
    val b: 42 = 42
    def c: String = "abc"
    def d: 42 = 42

    fun(a)
    fun(b)
    fun(c)
    fun(d)
    fun("abc")
    fun("abc": String)
    fun(s"abc${"def"}")

    fun2(a)
    fun2(b)
    fun2(c)
    fun2(d)
    fun2("abc")
    fun2("abc": String)
    fun2(s"abc${"def"}")

    type T
    type U = "abc"

    fun3[T]
    fun3[String]
    fun3["abc"]
    fun3[U]
  }

}
