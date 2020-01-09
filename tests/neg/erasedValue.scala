import scala.compiletime.erasedValue
object Test {
  def foo(a: Int): Int = 3
  foo(erasedValue[Int]) // error
}
