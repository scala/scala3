
import Macros._

object Test {
  def main(args: Array[String]): Unit = {
    val a: String = "abc"
    val b: 42 = 42
    def c: String = "abc"
    def d: 42 = 42

//  fun(a) // ERROR
//  fun(b) // ERROR
    fun(c) // error
    fun(d) // error
//  fun("abc") // ERROR
    fun("abc": String) // error
    fun(s"abc${"def"}") // error

//  fun2(a) // ERROR
//  fun2(b) // ERROR
    fun2(c) // error
    fun2(d) // error
//  fun2("abc") // ERROR
    fun2("abc": String) // error
    fun2(s"abc${"def"}") // error

//    type T
//    type U = "abc"
//
//    fun3[T] // ERROR
//    fun3[String] // ERROR
//    fun3["abc"] // ERROR
//    fun3[U] // ERROR
  }
// FIXME all the lines marked as ERROR have the wrong position on the three of the argument.
// they all have as source file this file but the span of `'x` in `fun` or `fun2`.
// see #6026 and #6027
}
