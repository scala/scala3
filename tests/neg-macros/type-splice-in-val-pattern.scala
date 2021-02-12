import scala.quoted.*
object Foo {
  def f(using q: Quotes) = {
    val t: Type[Int] = ???
    val '[ *:[$t] ] = ??? // error
  }
}