//> using options -language:experimental.erasedDefinitions

import scala.compiletime.erasedValue
object Test {
  def foo0(a: Int): Int = 3
  def foo1(erased a: Int): Int = 3
  foo0(erasedValue[Int]) // error
  foo1(erasedValue[Int]) // error
}
